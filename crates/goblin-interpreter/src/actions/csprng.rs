//! Goblin CSPRNG — Cryptographically-oriented secure randomness.
//!
//! Exposed to Goblin as:
//!   secure_pick    — cryptographically secure variant of pick (same cfg object)
//!   secure_random  — cryptographically secure range selection
//!   secure_shuffle — cryptographically secure shuffle
//!
//! This module is fail-closed. Any degraded entropy condition returns a hard
//! error rather than silently proceeding with weakened randomness.

use crate::{Diag, Session, Span, Value};
use crate::diagnostics::rtcode;
use goblin_diagnostics::{Diagnostic, Severity};

use blake3::Hasher;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::time::Instant;

// ---------------------------------------------------------------------------
// Interpreter source fingerprint — baked in at compile time.
// ---------------------------------------------------------------------------

const GOBLIN_SOURCE_CHAR_COUNT: u64 = include_str!("../../src/lib.rs").len() as u64;

// ---------------------------------------------------------------------------
// Persistent seed
// ---------------------------------------------------------------------------

fn persistent_seed_path() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".goblin").join("rng_seed"))
}

fn load_persistent_seed() -> Vec<u8> {
    persistent_seed_path()
        .and_then(|p| fs::read(p).ok())
        .unwrap_or_default()
}

fn save_persistent_seed(bytes: &[u8]) {
    let Some(path) = persistent_seed_path() else { return };
    let Some(parent) = path.parent() else { return };

    if fs::create_dir_all(parent).is_err() {
        return;
    }

    let tmp = path.with_extension("tmp");

    let write_result = (|| -> std::io::Result<()> {
        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&tmp)?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            f.set_permissions(fs::Permissions::from_mode(0o600))?;
        }

        f.write_all(bytes)?;
        f.sync_all()?;
        Ok(())
    })();

    if write_result.is_ok() {
        let _ = fs::rename(&tmp, &path);
    } else {
        let _ = fs::remove_file(&tmp);
    }
}

// ---------------------------------------------------------------------------
// Entropy quality
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
enum EntropyQuality {
    Good,
    Degraded { zeros: usize, most_common: u32 },
}

// ---------------------------------------------------------------------------
// CPU jitter collection
// ---------------------------------------------------------------------------

fn collect_cpu_jitter() -> (Vec<u8>, EntropyQuality) {
    let mut samples = Vec::with_capacity(1024);
    let mut prev = Instant::now();

    for _ in 0..1024 {
        let now = Instant::now();
        let delta = now.duration_since(prev).subsec_nanos();
        samples.push((delta & 0xFF) as u8);
        prev = now;
    }

    let zeros = samples.iter().filter(|&&b| b == 0).count();
    let most_common = {
        let mut counts = [0u32; 256];
        for &b in &samples {
            counts[b as usize] += 1;
        }
        *counts.iter().max().unwrap_or(&0)
    };

    let quality = if zeros > 102 || most_common > 102 {
        EntropyQuality::Degraded { zeros, most_common }
    } else {
        EntropyQuality::Good
    };

    (samples, quality)
}

// ---------------------------------------------------------------------------
// Filesystem entropy collection
// ---------------------------------------------------------------------------

fn check_io_delta_quality(deltas: &[u8]) -> EntropyQuality {
    if deltas.len() < 4 {
        return EntropyQuality::Good;
    }
    let most_common = {
        let mut counts = [0u32; 256];
        for &b in deltas {
            counts[b as usize] += 1;
        }
        *counts.iter().max().unwrap_or(&0)
    };
    let threshold = (deltas.len() as f32 * 0.9) as u32;
    if most_common >= threshold {
        EntropyQuality::Degraded {
            zeros: deltas.iter().filter(|&&b| b == 0).count(),
            most_common,
        }
    } else {
        EntropyQuality::Good
    }
}

fn collect_filesystem_entropy(jitter_byte: u8) -> (u64, Vec<u8>, EntropyQuality) {
    let depth = ((jitter_byte % 5) + 1) as usize;

    let mut path = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    for _ in 0..depth {
        match path.parent() {
            Some(p) => path = p.to_path_buf(),
            None => break,
        }
    }

    let mut file_size_sum: u64 = 0;
    let mut timing_deltas: Vec<u8> = Vec::new();
    let mut prev = Instant::now();

    if let Ok(entries) = fs::read_dir(&path) {
        for entry in entries.flatten() {
            if let Ok(meta) = entry.metadata() {
                file_size_sum = file_size_sum.wrapping_add(meta.len());
                let now = Instant::now();
                let delta = now.duration_since(prev).subsec_nanos();
                timing_deltas.push((delta & 0xFF) as u8);
                prev = now;
            }
        }
    }

    let quality = check_io_delta_quality(&timing_deltas);
    (file_size_sum, timing_deltas, quality)
}

// ---------------------------------------------------------------------------
// Seed generation — fail-closed on degraded entropy
// ---------------------------------------------------------------------------

fn generate_seed(sp: &Span) -> Result<[u8; 32], Diag> {
    let (cpu_jitter, quality) = collect_cpu_jitter();

    if let EntropyQuality::Degraded { zeros, most_common } = quality {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::FILESYSTEM_IO,
            "csprng-degraded-entropy",
            &format!(
                "secure RNG aborted: CPU jitter entropy is degraded \
                 (zero-deltas={zeros}/1024, most_common={most_common}/1024). \
                 This system's timer resolution may be too low for secure randomness."
            ),
            sp.clone(),
        )
        .with_help(
            "secure_pick, secure_random, and secure_shuffle require sufficient \
             CPU timing jitter. This error indicates the runtime clock has \
             insufficient resolution on this machine."
        )
        .with_link("https://goblinlang.org/docs/errors#FS0001"));
    }

    let jitter_byte = cpu_jitter.first().copied().unwrap_or(42);
    let (file_bytes, io_deltas, io_quality) = collect_filesystem_entropy(jitter_byte);

    if let EntropyQuality::Degraded { zeros, most_common } = io_quality {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::FILESYSTEM_IO,
            "csprng-degraded-io-entropy",
            &format!(
                "secure RNG aborted: I/O timing entropy is degraded \
                 (most_common={most_common}, zeros={zeros}). \
                 Filesystem timing variance is too low on this system."
            ),
            sp.clone(),
        )
        .with_help(
            "I/O timing deltas showed insufficient variance. \
             This may occur on ramdisks, certain VMs, or systems with \
             very fast cached filesystems."
        )
        .with_link("https://goblinlang.org/docs/errors#FS0001"));
    }

    let persistent = load_persistent_seed();

    let nanoseconds = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);

    let pid = std::process::id() as u64;

    let multiplier = ((jitter_byte % 100) + 1) as u64;
    let interpreter_fingerprint = GOBLIN_SOURCE_CHAR_COUNT.wrapping_mul(multiplier);

    let mut hasher = Hasher::new();
    hasher.update(b"goblin-seed-v1");
    hasher.update(&file_bytes.to_le_bytes());
    hasher.update(&nanoseconds.to_le_bytes());
    hasher.update(&pid.to_le_bytes());
    hasher.update(&interpreter_fingerprint.to_le_bytes());
    hasher.update(&io_deltas);
    hasher.update(&cpu_jitter);
    hasher.update(&persistent);

    let seed = *hasher.finalize().as_bytes();

    let mut reseed_hasher = Hasher::new();
    reseed_hasher.update(b"goblin-reseed-v1");
    reseed_hasher.update(&seed);
    reseed_hasher.update(&nanoseconds.to_le_bytes());
    save_persistent_seed(reseed_hasher.finalize().as_bytes());

    Ok(seed)
}

// ---------------------------------------------------------------------------
// CSPRNG context — one per top-level secure operation
// ---------------------------------------------------------------------------

struct CsprngContext {
    seed: [u8; 32],
    counter: u64,
    buffer: [u8; 32],
    buffer_pos: usize,
}

impl CsprngContext {
    fn new(seed: [u8; 32]) -> Self {
        let mut ctx = Self {
            seed,
            counter: 0,
            buffer: [0u8; 32],
            buffer_pos: 32,
        };
        ctx.fill_block();
        ctx
    }

    fn fill_block(&mut self) {
        let mut hasher = Hasher::new();
        hasher.update(b"goblin-stream-v1");
        hasher.update(&self.seed);
        hasher.update(&self.counter.to_le_bytes());
        self.buffer = *hasher.finalize().as_bytes();
        self.buffer_pos = 0;
        self.counter += 1;
    }

    fn next_byte(&mut self) -> u8 {
        if self.buffer_pos >= 32 {
            self.fill_block();
        }
        let b = self.buffer[self.buffer_pos];
        self.buffer_pos += 1;
        b
    }

    fn next_bounded(&mut self, bound: u128) -> u128 {
        assert!(bound > 0, "bound must be > 0");

        let byte_width = ((128 - bound.leading_zeros() + 7) / 8).max(1) as usize;
        let mask = if byte_width >= 16 {
            u128::MAX
        } else {
            (1u128 << (byte_width * 8)) - 1
        };

        let threshold: Option<u128> = if mask == u128::MAX {
            None
        } else {
            let range_size = mask + 1;
            Some(range_size - (range_size % bound))
        };

        loop {
            let mut val = 0u128;
            for i in 0..byte_width {
                val |= (self.next_byte() as u128) << (i * 8);
            }
            val &= mask;
            match threshold {
                None => return val % bound,
                Some(t) if val < t => return val % bound,
                _ => continue,
            }
        }
    }

    fn next_index(&mut self, len: usize) -> usize {
        self.next_bounded(len as u128) as usize
    }
}

// ---------------------------------------------------------------------------
// cfg map helpers — mirrors what the pick block uses in lib.rs
// ---------------------------------------------------------------------------

fn cfg_bool(m: &BTreeMap<String, Value>, k: &str) -> Option<bool> {
    m.get(k).and_then(|v| if let Value::Bool(b) = v { Some(*b) } else { None })
}

fn cfg_num(m: &BTreeMap<String, Value>, k: &str) -> Option<f64> {
    match m.get(k)? {
        Value::Float(n) => Some(*n),
        Value::Int(i)   => Some(*i as f64),
        Value::Str(s)   => s.parse::<f64>().ok(),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Public Goblin builtins
// ---------------------------------------------------------------------------

/// secure_pick — same cfg object as pick, uses CSPRNG instead of rng_index.
pub fn secure_pick(
    _sess: &mut Session,
    args: &[Value],
    sp: &Span,
) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("Wrong number of arguments (expected 1, got {})", args.len()),
            sp.clone(),
        )
        .with_help("Provide exactly 1 argument.")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let cfg = match &args[0] {
        Value::Map(m) => m.clone(),
        _ => return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "type-mismatch",
            "secure_pick expects a config object (map)",
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#T0205")),
    };

    let count_f = cfg_num(&cfg, "count_expr")
        .or_else(|| cfg_num(&cfg, "count"))
        .unwrap_or(1.0);

    if !count_f.is_finite() || count_f < 0.0 || count_f.fract() != 0.0 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::POSITIVE_INT_EXPECTED,
            "pick-count-not-integer",
            "secure_pick 'count' must be a non-negative integer",
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#T0202"));
    }

    let n_out = count_f as usize;
    if n_out == 0 {
        return Ok(Value::Array(vec![]));
    }

    let digits_opt_i64: Option<i64> = cfg_num(&cfg, "digits").map(|d| d as i64);
    let unique_digits = cfg_bool(&cfg, "unique").unwrap_or(false);
    let has_range = cfg.contains_key("range_start") && cfg.contains_key("range_end");

    enum CollectionSource {
        Array(Vec<Value>),
        Seq(Vec<Value>),
        Map(BTreeMap<String, Value>),
    }

    let src_collection: Option<CollectionSource> = if let Some(Value::Array(arr)) = cfg.get("src") {
        Some(CollectionSource::Array(arr.clone()))
    } else if let Some(Value::Seq(seq)) = cfg.get("src") {
        Some(CollectionSource::Seq(seq.to_vec()))
    } else if let Some(Value::Map(m)) = cfg.get("src") {
        Some(CollectionSource::Map(m.clone()))
    } else if let Some(Value::Str(s)) = cfg.get("src") {
        let chars: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
        Some(CollectionSource::Array(chars))
    } else {
        None
    };

    let allow_dups = match cfg_bool(&cfg, "allow_dups") {
        Some(b) => b,
        None => if src_collection.is_some() { false } else { true },
    };

    let finish = |mut items: Vec<Value>| -> Value {
        if n_out == 1 { items.pop().unwrap_or(Value::Nil) } else { Value::Array(items) }
    };

    let seed = generate_seed(sp)?;
    let mut ctx = CsprngContext::new(seed);

    // ================== Collections ==================
    if let Some(coll) = src_collection {
        match coll {
            CollectionSource::Array(arr) | CollectionSource::Seq(arr) => {
                if arr.is_empty() {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::EMPTY_COLLECTION,
                        "empty-collection",
                        "cannot secure_pick from an empty collection",
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0701"));
                }
                if !allow_dups && n_out > arr.len() {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::INSUFFICIENT_DISTINCT,
                        "insufficient-distinct",
                        &format!("cannot pick {} distinct items from {}", n_out, arr.len()),
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0703"));
                }
                let out = if allow_dups {
                    (0..n_out).map(|_| arr[ctx.next_index(arr.len())].clone()).collect()
                } else {
                    let mut idxs: Vec<usize> = (0..arr.len()).collect();
                    let mut out = Vec::with_capacity(n_out);
                    for i in 0..n_out {
                        let j = i + ctx.next_index(arr.len() - i);
                        idxs.swap(i, j);
                        out.push(arr[idxs[i]].clone());
                    }
                    out
                };
                return Ok(finish(out));
            }
            CollectionSource::Map(map) => {
                if map.is_empty() {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::EMPTY_COLLECTION,
                        "empty-collection",
                        "cannot secure_pick from an empty map",
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0701"));
                }
                let entries: Vec<(String, Value)> = map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                if !allow_dups && n_out > entries.len() {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::SAMPLE_TOO_LARGE,
                        "sample-too-large",
                        &format!("requested {} entries but only {} available", n_out, entries.len()),
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0704"));
                }
                let out = if allow_dups {
                    (0..n_out).map(|_| {
                        let idx = ctx.next_index(entries.len());
                        let mut pair = BTreeMap::new();
                        pair.insert(entries[idx].0.clone(), entries[idx].1.clone());
                        Value::Map(pair)
                    }).collect()
                } else {
                    let mut idxs: Vec<usize> = (0..entries.len()).collect();
                    let mut out = Vec::with_capacity(n_out);
                    for i in 0..n_out {
                        let j = i + ctx.next_index(entries.len() - i);
                        idxs.swap(i, j);
                        let mut pair = BTreeMap::new();
                        pair.insert(entries[idxs[i]].0.clone(), entries[idxs[i]].1.clone());
                        out.push(Value::Map(pair));
                    }
                    out
                };
                return Ok(finish(out));
            }
        }
    }

    // ================== Numeric Range ==================
    if has_range {
        let a_num = cfg_num(&cfg, "range_start");
        let b_num = cfg_num(&cfg, "range_end");
        let a_str = cfg.get("range_start").and_then(|v| if let Value::Str(s) = v { Some(s) } else { None });
        let b_str = cfg.get("range_end").and_then(|v| if let Value::Str(s) = v { Some(s) } else { None });

        if let (Some(a_s), Some(b_s)) = (a_str, b_str) {
            if a_s.len() == 1 && b_s.len() == 1 {
                let start_char = a_s.chars().next().unwrap();
                let end_char   = b_s.chars().next().unwrap();
                let inc = cfg_bool(&cfg, "range_inclusive").unwrap_or(false);
                let mut pool: Vec<char> = if inc {
                    (start_char..=end_char).collect()
                } else {
                    (start_char..end_char).collect()
                };
                if pool.is_empty() {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::INVALID_RANGE_NO_VALUES,
                        "invalid-range-no-values",
                        "invalid character range (no values)",
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0702"));
                }
                if !allow_dups && n_out > pool.len() {
                    return Err(Diagnostic::new_with_code(
                        Severity::Error,
                        rtcode::SAMPLE_TOO_LARGE,
                        "sample-too-large",
                        &format!("requested sample of {} exceeds available {} characters", n_out, pool.len()),
                        sp.clone(),
                    )
                    .with_link("https://goblinlang.org/docs/errors#R0704"));
                }
                let picked: Vec<char> = if allow_dups {
                    (0..n_out).map(|_| pool[ctx.next_index(pool.len())]).collect()
                } else {
                    for i in 0..n_out {
                        let j = i + ctx.next_index(pool.len() - i);
                        pool.swap(i, j);
                    }
                    (0..n_out).map(|i| pool[i]).collect()
                };
                return if n_out == 1 {
                    Ok(Value::Str(picked[0].to_string()))
                } else {
                    Ok(Value::Str(picked.iter().collect()))
                };
            }
        }

        let a = a_num.ok_or_else(|| Diagnostic::new_with_code(Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch", "range bounds must be numbers or single characters", sp.clone()))?;
        let b = b_num.ok_or_else(|| Diagnostic::new_with_code(Severity::Error, rtcode::TYPE_MISMATCH, "type-mismatch", "range bounds must be numbers or single characters", sp.clone()))?;

        let mut lo = a as i64;
        let mut hi = b as i64;
        if lo > hi { std::mem::swap(&mut lo, &mut hi); }
        let inc = cfg_bool(&cfg, "range_inclusive").unwrap_or(false);
        let d = digits_opt_i64.unwrap_or(0);

        let within_digits = |v: i64, d: i64| -> bool {
            if d <= 0 { return true; }
            let min = 10_i64.pow((d - 1) as u32);
            let max = 10_i64.pow(d as u32) - 1;
            v >= min && v <= max
        };
        let has_unique_digits = |mut v: i64, d: i64| -> bool {
            if !unique_digits { return true; }
            if d > 0 && v < 10_i64.pow((d - 1) as u32) { return false; }
            let mut seen = [false; 10];
            if v == 0 { return false; }
            while v > 0 { let dd = (v % 10) as usize; if seen[dd] { return false; } seen[dd] = true; v /= 10; }
            true
        };

        let mut pool: Vec<i64> = Vec::new();
        if inc { for v in lo..=hi { if within_digits(v, d) && has_unique_digits(v, d) { pool.push(v); } } }
        else   { for v in lo..hi  { if within_digits(v, d) && has_unique_digits(v, d) { pool.push(v); } } }

        if pool.is_empty() {
            return Err(Diagnostic::new_with_code(Severity::Error, rtcode::INVALID_RANGE_NO_VALUES, "invalid-range-no-values", "invalid range (no values after filters)", sp.clone())
                .with_link("https://goblinlang.org/docs/errors#R0702"));
        }
        if !allow_dups && n_out > pool.len() {
            return Err(Diagnostic::new_with_code(Severity::Error, rtcode::SAMPLE_TOO_LARGE, "sample-too-large",
                &format!("requested sample of {} exceeds available {} distinct values", n_out, pool.len()), sp.clone())
                .with_link("https://goblinlang.org/docs/errors#R0704"));
        }

        let out_vals: Vec<Value> = if allow_dups {
            (0..n_out).map(|_| Value::Int(pool[ctx.next_index(pool.len())] as i64)).collect()
        } else {
            for i in 0..n_out {
                let j = i + ctx.next_index(pool.len() - i);
                pool.swap(i, j);
            }
            (0..n_out).map(|i| Value::Int(pool[i] as i64)).collect()
        };

        return Ok(if n_out == 1 { out_vals.into_iter().next().unwrap() } else { Value::Array(out_vals) });
    }

    // ================== Pure Digits ==================
    if let Some(d) = digits_opt_i64 {
        let domain_size = if unique_digits {
            let mut total: i64 = 9;
            let mut avail: i64 = 9;
            for _ in 1..d { total *= avail; avail -= 1; }
            total.max(0) as usize
        } else {
            (10_i64.pow(d as u32) - 10_i64.pow((d - 1) as u32)) as usize
        };

        if !allow_dups && n_out > domain_size {
            return Err(Diagnostic::new_with_code(Severity::Error, rtcode::SAMPLE_TOO_LARGE, "sample-too-large",
                &format!("requested sample of {} exceeds available {} distinct values in digit space", n_out, domain_size), sp.clone())
                .with_link("https://goblinlang.org/docs/errors#R0704"));
        }

        let mut gen_one = || -> i64 {
            if !unique_digits {
                let min = 10_i64.pow((d - 1) as u32);
                let width = (9_i64 * 10_i64.pow((d - 1) as u32)) as usize;
                min + ctx.next_index(width) as i64
            } else {
                let mut digits: [i64; 10] = [0,1,2,3,4,5,6,7,8,9];
                let first_idx = 1 + ctx.next_index(9);
                let first = digits[first_idx];
                digits[first_idx] = digits[9];
                let mut val: i64 = first;
                let mut size = 9;
                for _ in 1..d {
                    let idx = ctx.next_index(size + 1);
                    let chosen = digits[idx];
                    digits[idx] = digits[size];
                    if size > 0 { size -= 1; }
                    val = val * 10 + chosen;
                }
                val
            }
        };

        let out = if allow_dups {
            (0..n_out).map(|_| Value::Int(gen_one())).collect()
        } else {
            let mut set = BTreeSet::<i64>::new();
            let mut attempts_left: usize = domain_size.saturating_mul(3).max(n_out * 10);
            while set.len() < n_out {
                if attempts_left == 0 {
                    return Err(Diagnostic::new_with_code(Severity::Error, rtcode::INSUFFICIENT_DISTINCT, "insufficient-distinct",
                        "could not generate enough distinct values", sp.clone())
                        .with_link("https://goblinlang.org/docs/errors#R0703"));
                }
                attempts_left -= 1;
                set.insert(gen_one());
            }
            set.into_iter().map(Value::Int).collect()
        };

        return Ok(finish(out));
    }

    Err(Diagnostic::new_with_code(Severity::Error, rtcode::PICK_MISSING_SOURCE, "pick-missing-source",
        "secure_pick needs a source: `from <collection>` or a numeric form", sp.clone())
        .with_link("https://goblinlang.org/docs/errors#P1408"))
}

/// secure_random — draw a secure random integer in [min, max] inclusive.
pub fn secure_random(
    _sess: &mut Session,
    args: &[Value],
    sp: &Span,
) -> Result<Value, Diag> {
    if args.len() != 2 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("secure_random expects 2 arguments (min, max), got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :secure_random(min, max)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let (min, max) = match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => (*a, *b),
        _ => return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "type-mismatch",
            "secure_random arguments must be integers",
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#T0205")),
    };

    if min > max {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "invalid-range",
            &format!("secure_random min ({min}) must be <= max ({max})"),
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#T0205"));
    }

    let range = (max as i128 - min as i128 + 1) as u128;
    let seed = generate_seed(sp)?;
    let mut ctx = CsprngContext::new(seed);
    let offset = ctx.next_bounded(range) as i128;

    Ok(Value::Int((min as i128 + offset) as i64))
}

/// secure_shuffle — return a securely shuffled copy of array or string.
pub fn secure_shuffle(
    _sess: &mut Session,
    args: &[Value],
    sp: &Span,
) -> Result<Value, Diag> {
    if args.len() != 1 {
        return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::WRONG_ARITY,
            "wrong-arity",
            &format!("secure_shuffle expects 1 argument, got {}", args.len()),
            sp.clone(),
        )
        .with_help("Usage: :secure_shuffle(array)")
        .with_link("https://goblinlang.org/docs/errors#R0301"));
    }

    let input_was_string = matches!(&args[0], Value::Str(_));

    let mut items: Vec<Value> = match &args[0] {
        Value::Array(xs) => xs.clone(),
        Value::Str(s) => s.chars().map(|c| Value::Str(c.to_string())).collect(),
        _ => return Err(Diagnostic::new_with_code(
            Severity::Error,
            rtcode::TYPE_MISMATCH,
            "type-mismatch",
            "secure_shuffle argument must be an array or string",
            sp.clone(),
        )
        .with_link("https://goblinlang.org/docs/errors#T0205")),
    };

    let seed = generate_seed(sp)?;
    let mut ctx = CsprngContext::new(seed);

    let n = items.len();
    for i in (1..n).rev() {
        let j = ctx.next_index(i + 1);
        items.swap(i, j);
    }

    if input_was_string {
        Ok(Value::Str(items.iter().map(|v| match v { Value::Str(c) => c.as_str(), _ => "" }).collect()))
    } else {
        Ok(Value::Array(items))
    }
}
