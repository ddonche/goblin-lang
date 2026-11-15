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

pub fn write_png(path: &str, width: i64, height: i64, pixels: &Value) -> Result<Value, GoblinError> {
    if width <= 0 || height <= 0 || width > 1 << 20 || height > 1 << 20 {
        return Err(err(format!("R1270: write_png: size {width} x {height} is not a picture size")));
    }
    let owned;
    let list: &[Value] = match pixels {
        Value::Array(a) => a,
        Value::Collection(c) if !c.is_map() => { owned = crate::collections::to_vec(c); &owned }
        Value::Frozen(f) => return write_png(path, width, height, &f.value),
        other => return Err(err(format!("R1270: write_png: pixels is {}, not a list of colors", other.type_name()))),
    };
    let (w, h) = (width as usize, height as usize);
    if list.len() != w * h {
        return Err(err(format!(
            "R1270: write_png: {w} x {h} needs {} pixels, got {}", w * h, list.len())));
    }
    let mut colors = Vec::with_capacity(list.len());
    let mut clear = false;
    for (i, v) in list.iter().enumerate() {
        let c = color_of(v, i)?;
        clear |= c[3] != 255;
        colors.push(c);
    }
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
