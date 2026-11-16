//! `:write_png!(path, width, height, pixels)` and `:read_png(path)`.
//!
//! A picture is a flat list of colors, row by row (`i = y * width + x`).
//! Each color is a string `"#rrggbb"` or `"#rrggbbaa"`, or nil for a clear
//! pixel. `:read_png` gives back `{"width", "height", "pixels"}` in the same
//! form, so a picture read in can be written out unchanged.
//!
//! The PNG coder is written here (8-bit, no interlacing on read) so no image
//! library is needed; deflate comes from flate2, which checkpoints already use.

use crate::error::GoblinError;
use crate::value::Value;
use std::io::{Read, Write};

fn err(msg: String) -> GoblinError { GoblinError::Runtime(msg) }

/// One color as RGBA, from `"#rrggbb"`, `"#rrggbbaa"` or nil (clear).
fn color_of(v: &Value, i: usize) -> Result<[u8; 4], GoblinError> {
    let s: &str = match v {
        Value::Nil => return Ok([0, 0, 0, 0]),
        Value::Str(s) => s,
        other => return Err(err(format!(
            "R1270: write_png: pixel {i} is {}, not a color like \"#rrggbb\"", other.type_name()))),
    };
    let hex = s.strip_prefix('#').unwrap_or("");
    let byte = |k: usize| u8::from_str_radix(&hex[k..k + 2], 16).ok();
    let parsed = match hex.len() {
        6 if hex.is_ascii() => (|| Some([byte(0)?, byte(2)?, byte(4)?, 255]))(),
        8 if hex.is_ascii() => (|| Some([byte(0)?, byte(2)?, byte(4)?, byte(6)?]))(),
        _ => None,
    };
    parsed.ok_or_else(|| err(format!(
        "R1270: write_png: pixel {i} is \"{s}\", not a color like \"#rrggbb\" or \"#rrggbbaa\"")))
}

fn chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
    out.extend_from_slice(&(data.len() as u32).to_be_bytes());
    let start = out.len();
    out.extend_from_slice(kind);
    out.extend_from_slice(data);
    let mut crc = flate2::Crc::new();
    crc.update(&out[start..]);
    out.extend_from_slice(&crc.sum().to_be_bytes());
}

/// Drawing options: `scale` (output pixels per input pixel, default 1),
/// `smooth` (curved edges between colors; default on when scale > 1),
/// `border` (a color for thin lines where colors meet; default none) and
/// `border_width` (in output pixels, default 1.5).
pub struct Look { pub scale: usize, pub smooth: bool, pub border: Option<[u8; 4]>, pub border_width: f64 }

impl Look {
    pub fn plain() -> Self { Look { scale: 1, smooth: false, border: None, border_width: 1.5 } }

    pub fn from_options(v: &Value) -> Result<Self, GoblinError> {
        let mut look = Look::plain();
        let mut smooth = None;
        let entries: Vec<(String, Value)> = match v {
            Value::Map(m) => m.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            Value::MapOrd(m) => m.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            Value::Collection(c) if c.is_map() => crate::collections::to_pairs(c).into_iter()
                .filter_map(|(k, v)| if let Value::Str(s) = k { Some((s.to_string(), v)) } else { None }).collect(),
            Value::Frozen(f) => return Look::from_options(&f.value),
            other => return Err(err(format!("R1270: write_png: the options are {}, not a map", other.type_name()))),
        };
        for (k, v) in entries {
            match (k.as_str(), &v) {
                ("scale", Value::Int(n)) if (1..=64).contains(n) => look.scale = *n as usize,
                ("scale", _) => return Err(err("R1270: write_png: `scale` is a whole number from 1 to 64".into())),
                ("smooth", Value::Bool(b)) => smooth = Some(*b),
                ("smooth", _) => return Err(err("R1270: write_png: `smooth` is true or false".into())),
                ("border", Value::Nil) => look.border = None,
                ("border", c) => look.border = Some(color_of(c, 0).map_err(|_| err("R1270: write_png: `border` is a color like \"#rrggbb\"".into()))?),
                ("border_width", Value::Int(n)) if *n >= 0 => look.border_width = *n as f64,
                ("border_width", Value::Float(f)) if *f >= 0.0 => look.border_width = *f,
                ("border_width", _) => return Err(err("R1270: write_png: `border_width` is a number of pixels".into())),
                (other, _) => return Err(err(format!("R1270: write_png: no option `{other}` (scale, smooth, border, border_width)"))),
            }
        }
        look.smooth = smooth.unwrap_or(look.scale > 1);
        Ok(look)
    }
}

fn blend(a: [u8; 4], b: [u8; 4], t: f64) -> [u8; 4] {
    let mut o = [0u8; 4];
    for k in 0..4 { o[k] = (a[k] as f64 * (1.0 - t) + b[k] as f64 * t).round() as u8; }
    o
}

/// The picture drawn larger. Each output pixel weighs the input pixels
/// around it (a Gaussian bell, sigma 0.7 input pixels) by color; the color
/// with the most weight fills it. Where two colors are near even, the edge
/// is drawn smooth (blended over one output pixel) and, with a border color,
/// a line of `border_width` pixels is laid along it.
fn draw_large(colors: &[[u8; 4]], w: usize, h: usize, look: &Look) -> (Vec<[u8; 4]>, usize, usize) {
    let s = look.scale;
    let (ow, oh) = (w * s, h * s);
    if !look.smooth && look.border.is_none() {
        let mut out = Vec::with_capacity(ow * oh);
        for oy in 0..oh { for ox in 0..ow { out.push(colors[(oy / s) * w + ox / s]); } }
        return (out, ow, oh);
    }
    // Colors to small numbers, so weighing compares numbers.
    let mut ids: std::collections::HashMap<[u8; 4], u16> = std::collections::HashMap::new();
    let mut palette: Vec<[u8; 4]> = Vec::new();
    let cell: Vec<u16> = colors.iter().map(|c| *ids.entry(*c).or_insert_with(|| { palette.push(*c); (palette.len() - 1) as u16 })).collect();
    let sigma: f64 = if look.smooth { 0.7 } else { 0.35 };
    let r = 2i64;
    let mut first = vec![0u16; ow * oh];
    let mut second = vec![0u16; ow * oh];
    let mut margin = vec![1.0f32; ow * oh];
    let mut wsum: Vec<f64> = vec![0.0; palette.len()];
    let mut touched: Vec<u16> = Vec::new();
    for oy in 0..oh {
        let v = (oy as f64 + 0.5) / s as f64 - 0.5;
        for ox in 0..ow {
            let u = (ox as f64 + 0.5) / s as f64 - 0.5;
            let (cx, cy) = (u.round() as i64, v.round() as i64);
            for dy in -r..=r {
                let y = cy + dy;
                if y < 0 || y >= h as i64 { continue; }
                for dx in -r..=r {
                    let x = cx + dx;
                    if x < 0 || x >= w as i64 { continue; }
                    let (ddx, ddy) = (x as f64 - u, y as f64 - v);
                    let wt = (-(ddx * ddx + ddy * ddy) / (2.0 * sigma * sigma)).exp();
                    let id = cell[y as usize * w + x as usize];
                    if wsum[id as usize] == 0.0 { touched.push(id); }
                    wsum[id as usize] += wt;
                }
            }
            let (mut a, mut wa, mut b, mut wb) = (0u16, -1.0f64, 0u16, -1.0f64);
            for &id in &touched {
                let wt = wsum[id as usize];
                if wt > wa { b = a; wb = wa; a = id; wa = wt; } else if wt > wb { b = id; wb = wt; }
            }
            for &id in &touched { wsum[id as usize] = 0.0; }
            touched.clear();
            let i = oy * ow + ox;
            first[i] = a;
            if wb > 0.0 { second[i] = b; margin[i] = ((wa - wb) / (wa + wb)) as f32; } else { second[i] = a; }
        }
    }
    // Distance to the edge in output pixels: margin over its slope.
    let mut out = Vec::with_capacity(ow * oh);
    for oy in 0..oh {
        for ox in 0..ow {
            let i = oy * ow + ox;
            let (a, b, m) = (first[i], second[i], margin[i] as f64);
            if a == b { out.push(palette[a as usize]); continue; }
            let at = |x: usize, y: usize| margin[y * ow + x] as f64;
            let gx = if ox > 0 && ox + 1 < ow { (at(ox + 1, oy) - m).abs().max((m - at(ox - 1, oy)).abs()) } else { 0.0 };
            let gy = if oy > 0 && oy + 1 < oh { (at(ox, oy + 1) - m).abs().max((m - at(ox, oy - 1)).abs()) } else { 0.0 };
            let g = (gx * gx + gy * gy).sqrt();
            let d = if g > 1e-9 { m / g } else { f64::INFINITY };
            let mut c = palette[a as usize];
            if look.smooth && d < 1.0 { c = blend(palette[b as usize], c, 0.5 + 0.5 * d); }
            if let Some(line) = look.border {
                let cover = (look.border_width / 2.0 + 0.5 - d).clamp(0.0, 1.0) * (line[3] as f64 / 255.0);
                if cover > 0.0 { c = blend(c, [line[0], line[1], line[2], 255], cover); }
            }
            out.push(c);
        }
    }
    (out, ow, oh)
}

pub fn write_png(path: &str, width: i64, height: i64, pixels: &Value) -> Result<Value, GoblinError> {
    write_png_with(path, width, height, pixels, &Look::plain())
}

pub fn write_png_with(path: &str, width: i64, height: i64, pixels: &Value, look: &Look) -> Result<Value, GoblinError> {
    if width <= 0 || height <= 0 || width > 1 << 20 || height > 1 << 20 {
        return Err(err(format!("R1270: write_png: size {width} x {height} is not a picture size")));
    }
    let owned;
    let list: &[Value] = match pixels {
        Value::Array(a) => a,
        Value::Collection(c) if !c.is_map() => { owned = crate::collections::to_vec(c); &owned }
        Value::Frozen(f) => return write_png_with(path, width, height, &f.value, look),
        other => return Err(err(format!("R1270: write_png: pixels is {}, not a list of colors", other.type_name()))),
    };
    let (w, h) = (width as usize, height as usize);
    if list.len() != w * h {
        return Err(err(format!(
            "R1270: write_png: {w} x {h} needs {} pixels, got {}", w * h, list.len())));
    }
    let mut colors = Vec::with_capacity(list.len());
    for (i, v) in list.iter().enumerate() { colors.push(color_of(v, i)?); }
    let (colors, w, h) = if look.scale > 1 || look.smooth || look.border.is_some() { draw_large(&colors, w, h, look) } else { (colors, w, h) };
    let clear = colors.iter().any(|c| c[3] != 255);
    let per = if clear { 4 } else { 3 };
    let mut raw = Vec::with_capacity(h * (1 + w * per));
    for row in colors.chunks(w) {
        raw.push(0); // no filter
        for c in row { raw.extend_from_slice(&c[..per]); }
    }
    let mut z = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
    z.write_all(&raw).map_err(|e| err(format!("write_png: {e}")))?;
    let idat = z.finish().map_err(|e| err(format!("write_png: {e}")))?;

    let mut out = b"\x89PNG\r\n\x1a\n".to_vec();
    let mut ihdr = Vec::with_capacity(13);
    ihdr.extend_from_slice(&(w as u32).to_be_bytes());
    ihdr.extend_from_slice(&(h as u32).to_be_bytes());
    ihdr.extend_from_slice(&[8, if clear { 6 } else { 2 }, 0, 0, 0]);
    chunk(&mut out, b"IHDR", &ihdr);
    chunk(&mut out, b"IDAT", &idat);
    chunk(&mut out, b"IEND", &[]);
    std::fs::write(path, out).map_err(|e| err(format!("write_png: {path}: {e}")))?;
    Ok(Value::Nil)
}

fn paeth(a: u8, b: u8, c: u8) -> u8 {
    let p = a as i16 + b as i16 - c as i16;
    let (pa, pb, pc) = ((p - a as i16).abs(), (p - b as i16).abs(), (p - c as i16).abs());
    if pa <= pb && pa <= pc { a } else if pb <= pc { b } else { c }
}

pub fn read_png(path: &str) -> Result<Value, GoblinError> {
    let bad = |why: &str| err(format!("R1271: read_png: {path}: {why}"));
    let data = std::fs::read(path).map_err(|e| err(format!("read_png: {path}: {e}")))?;
    if data.len() < 8 || &data[..8] != b"\x89PNG\r\n\x1a\n" { return Err(bad("not a PNG file")); }
    let (mut w, mut h, mut depth, mut kind, mut interlace) = (0usize, 0usize, 0u8, 0u8, 0u8);
    let mut palette: Vec<[u8; 4]> = Vec::new();
    let mut idat = Vec::new();
    let mut at = 8;
    while at + 8 <= data.len() {
        let len = u32::from_be_bytes(data[at..at + 4].try_into().unwrap()) as usize;
        let name = &data[at + 4..at + 8];
        let Some(body) = data.get(at + 8..at + 8 + len) else { return Err(bad("cut short")) };
        match name {
            b"IHDR" if len >= 13 => {
                w = u32::from_be_bytes(body[0..4].try_into().unwrap()) as usize;
                h = u32::from_be_bytes(body[4..8].try_into().unwrap()) as usize;
                depth = body[8]; kind = body[9]; interlace = body[12];
            }
            b"PLTE" => palette = body.chunks(3).filter(|c| c.len() == 3).map(|c| [c[0], c[1], c[2], 255]).collect(),
            b"tRNS" if kind == 3 => for (i, a) in body.iter().enumerate() { if let Some(p) = palette.get_mut(i) { p[3] = *a; } },
            b"IDAT" => idat.extend_from_slice(body),
            b"IEND" => break,
            _ => {}
        }
        at += 12 + len;
    }
    if w == 0 || h == 0 { return Err(bad("no picture size")); }
    if depth != 8 { return Err(bad(&format!("{depth}-bit color is not read (8-bit only)"))); }
    if interlace != 0 { return Err(bad("interlaced pictures are not read")); }
    let per = match kind { 0 => 1, 2 => 3, 3 => 1, 4 => 2, 6 => 4, _ => return Err(bad("unknown color type")) };
    let mut raw = Vec::new();
    flate2::read::ZlibDecoder::new(&idat[..]).read_to_end(&mut raw).map_err(|_| bad("damaged picture data"))?;
    let stride = w * per;
    if raw.len() < h * (stride + 1) { return Err(bad("cut short")); }
    let mut px = vec![0u8; h * stride];
    for y in 0..h {
        let filter = raw[y * (stride + 1)];
        let line = &raw[y * (stride + 1) + 1..(y + 1) * (stride + 1)];
        for x in 0..stride {
            let a = if x >= per { px[y * stride + x - per] } else { 0 };
            let b = if y > 0 { px[(y - 1) * stride + x] } else { 0 };
            let c = if x >= per && y > 0 { px[(y - 1) * stride + x - per] } else { 0 };
            px[y * stride + x] = match filter {
                0 => line[x],
                1 => line[x].wrapping_add(a),
                2 => line[x].wrapping_add(b),
                3 => line[x].wrapping_add(((a as u16 + b as u16) / 2) as u8),
                4 => line[x].wrapping_add(paeth(a, b, c)),
                _ => return Err(bad("damaged picture data")),
            };
        }
    }
    let mut pixels = Vec::with_capacity(w * h);
    for p in px.chunks(per) {
        let c = match kind {
            0 => [p[0], p[0], p[0], 255],
            2 => [p[0], p[1], p[2], 255],
            3 => *palette.get(p[0] as usize).ok_or_else(|| bad("color outside the palette"))?,
            4 => [p[0], p[0], p[0], p[1]],
            _ => [p[0], p[1], p[2], p[3]],
        };
        let s = if c[3] == 255 { format!("#{:02x}{:02x}{:02x}", c[0], c[1], c[2]) }
                else { format!("#{:02x}{:02x}{:02x}{:02x}", c[0], c[1], c[2], c[3]) };
        pixels.push(Value::Str(s.into()));
    }
    let mut m = std::collections::BTreeMap::new();
    m.insert("width".to_string(), Value::Int(w as i64));
    m.insert("height".to_string(), Value::Int(h as i64));
    m.insert("pixels".to_string(), Value::Array(pixels));
    Ok(Value::Map(m))
}
