// crates/goblin-interpreter/src/actions/ipsum.rs
use crate::{Session, Value, Diag, Span};

const GOBLIN_WORDS: &[&str] = &[
    "grak", "snortum", "jabberfang", "muck-pit", "shinies", "grotto", "cave", "rev", "pain",
    "skitter", "chitter", "scrapclave", "bog-brew", "stash", "fang", "hidden", "treasures", "keeper",
    "tether", "hoard", "stash", "skrag", "bones", "gold", "lock", "club", "hut", "clan", "warrior",
    "graveyard", "coin", "loot", "glam-glam", "mutton", "hock", "priest", "priestess", "magic", "sorceror",
    "boom", "goblin", "filch", "snicker", "clank", "rusty", "witch", "wizard", "feast", "supper", "rotten",
    "zint","krell","vosh","tarrak","brumm","gless","sharn","dorr", "ogre", "giant", "beast", "monster",
    "jelt","murn","plesh","quarn","rellik","sann","torm","vellix", "human", "elf", "dwarf", "hobbit", "orc",
    "zorn","brask","chull","drimm","fess","gann","hork","jinx", "kress", "troll", "gremlin", "imp", "demons",
    "lorn","mizz","neth","phex","quill","rusk","seth","tazz","vorn", "bats", "spider", "snake", "pirates",
    "wex","yorn","zess","blint","crall","druss","frenn","grish", "halth", "fishy", "me", "mines", "wanting",
    "jorr","krinn","lusk","morth","nall","prill","quess","rinth", "took", "ghouls", "coffin", "swampy",
    "soll","trenn","vask","winn","xell","zall", "zi","kor","vu","taz", "him" , "her", "kiddie", "itsy",
    "gle","shU","dor","je","mur","qar","re","sax","to","vex","zo","chu", "bitsy", "welcome", "wishy", "sparkle",
    "fex","ga","hor","ji","kru","lo","miz","ne","phi","qi","ruz","se","tav", "sleepy", "mad", "hurt", "come",
    "vo","wex","yo","zes","cra","dr","fri","gr","hal","jo","kri","lu","mor", "cough", "sick", "taken", "lost",
    "na","pri","qu","rin","so","tru","va","wil","xe","zal", "away", "gone", "green", "yellow", "red", "blue",
    "purple", "brown", "black", "white", "pink", "orange", "totem", "power", "thinky", "good", "tower",
    "knife", "stick", "runny", "long", "time", "many-many", "get", "hooky", "doggums", "yum-yum", "lashy",
    "terribles",
];

const COMMON_WORDS: &[&str] = &[
    "the", "and", "in", "under", "over", "through", "with", "from", "above", "if",
    "of", "a", "this", "that", "little", "old", "hungry", "loud", "under", "go",
    "quiet", "strange", "broken", "big", "tiny", "at", "there", "be", "for", "out",
];

fn clamp_count(count: i64, default: usize) -> usize {
    if count <= 0 {
        default
    } else {
        count as usize
    }
}

/// Very small deterministic LCG so output *looks* random but is stable for tests.
fn lcg_next(seed: &mut u64) -> u32 {
    *seed = seed
        .wrapping_mul(6364136223846793005)
        .wrapping_add(1);
    (*seed >> 32) as u32
}

fn rand_range(seed: &mut u64, max: usize) -> usize {
    if max == 0 {
        0
    } else {
        (lcg_next(seed) as usize) % max
    }
}

/// Generate total_words of goblin-ish text:
/// - variable sentence lengths (4..14 words)
/// - each sentence starts with a capital
/// - mixed ., !, ? at sentence ends
fn generate_words(total_words: usize, seed: &mut u64) -> String {
    if total_words == 0 {
        return String::new();
    }

    let mut out = String::new();
    let mut words_emitted = 0usize;
    let mut at_sentence_start = true;

    // First sentence target length: 4..14 words
    let mut words_until_break = 4 + rand_range(seed, 11);

    while words_emitted < total_words {
        let i = words_emitted;

        // 1 in 4 words are common; rest goblin-y
        let pool = if i % 4 == 0 { COMMON_WORDS } else { GOBLIN_WORDS };
        let idx = rand_range(seed, pool.len());
        let mut w = pool[idx].to_string();

        // Capitalize at the start of each sentence
        if at_sentence_start {
            if let Some(first) = w.chars().next() {
                let mut buf = String::new();
                buf.push(first.to_ascii_uppercase());
                buf.push_str(&w[first.len_utf8()..]);
                w = buf;
            }
            at_sentence_start = false;
        }

        if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(&w);

        words_emitted += 1;
        words_until_break = words_until_break.saturating_sub(1);

        let last_word = words_emitted == total_words;
        let should_break = words_until_break == 0 || last_word;

        if should_break {
            // Choose punctuation: mostly '.', sometimes '!' or '?'
            let roll = rand_range(seed, 10);
            let punct = if roll < 7 {
                '.'
            } else if roll < 9 {
                '!'
            } else {
                '?'
            };
            out.push(punct);

            if !last_word {
                // Start a new sentence
                at_sentence_start = true;
                // Next sentence length 4..14
                words_until_break = 4 + rand_range(seed, 11);
            }
        }
    }

    out
}

/// ipsum() / ipsum(n)
/// - no args       → 100 words
/// - 1 int arg     → that many words (<=0 falls back to 100)
pub fn ipsum(_sess: &mut Session, args: &[Value], _sp: &Span) -> Result<Value, Diag> {
    let default_words = 100usize;

    let words = match args.first() {
        None => default_words,
        Some(v) => match v {
            Value::Int(n) => clamp_count(*n, default_words),
            _ => default_words, // non-int → fallback
        },
    };

    // Seed based on word count so :ipsum(40) is deterministic
    let mut seed = (words as u64)
        .wrapping_mul(0x9E37_79B9_7F4A_7C15);

    let text = generate_words(words, &mut seed);
    Ok(Value::Str(text))
}

/// ipsum_sentences(n)
/// Very simple: n sentences * ~12 words each (but actual sentence lengths vary).
pub fn ipsum_sentences(
    _sess: &mut Session,
    args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    let default_sentences = 3usize;
    let avg_words_per_sentence = 12usize;

    let sentences = match args.first() {
        None => default_sentences,
        Some(v) => match v {
            Value::Int(n) => clamp_count(*n, default_sentences),
            _ => default_sentences,
        },
    };

    let total_words = sentences * avg_words_per_sentence;

    let mut seed = (sentences as u64)
        .wrapping_mul(0x9E37_79B9_7F4A_7C15);

    let text = generate_words(total_words, &mut seed);
    Ok(Value::Str(text))
}

/// ipsum_paragraphs(n)
/// n paragraphs * ~5 sentences each * ~12 words each (lengths vary internally).
pub fn ipsum_paragraphs(
    _sess: &mut Session,
    args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    let default_paragraphs = 3usize;

    let paragraphs = match args.first() {
        None => default_paragraphs,
        Some(v) => match v {
            Value::Int(n) => clamp_count(*n, default_paragraphs),
            _ => default_paragraphs,
        },
    };

    let mut out = String::new();

    // One seed for the whole block, so paras differ but are deterministic
    let mut seed = (paragraphs as u64)
        .wrapping_mul(0x9E37_79B9_7F4A_7C15);

    for p in 0..paragraphs {
        if p > 0 {
            out.push_str("\n\n");
        }

        // words per paragraph: 30..=100
        let para_words = 30 + rand_range(&mut seed, 71); // 0..70 → 30..100

        let para = generate_words(para_words, &mut seed);
        out.push_str(&para);
    }

    Ok(Value::Str(out))
}

/// ipsum_full(words_per_sentence, sentences_per_paragraph, paragraphs)
/// Shapes the text:
/// - arg 1: words per sentence (avg)
/// - arg 2: sentences per paragraph (avg)
/// - arg 3: paragraphs (exact)
pub fn ipsum_full(
    _sess: &mut Session,
    args: &[Value],
    _sp: &Span,
) -> Result<Value, Diag> {
    let default_wps = 12usize;
    let default_spp = 5usize;
    let default_pars = 3usize;

    let wps = match args.get(0) {
        Some(v) => match v {
            Value::Int(n) => clamp_count(*n, default_wps),
            _ => default_wps,
        },
        None => default_wps,
    };

    let spp = match args.get(1) {
        Some(v) => match v {
            Value::Int(n) => clamp_count(*n, default_spp),
            _ => default_spp,
        },
        None => default_spp,
    };

    let pars = match args.get(2) {
        Some(v) => match v {
            Value::Int(n) => clamp_count(*n, default_pars),
            _ => default_pars,
        },
        None => default_pars,
    };

    let mut out = String::new();

    // Seed incorporates all three shape params
    let mut seed = (wps as u64)
        .wrapping_mul(0x9E37_79B9_7F4A_7C15)
        ^ ((spp as u64) << 21)
        ^ ((pars as u64) << 42);

    // Base "target" size for a paragraph
    let base_words = wps * spp;
    // Allow ± ~33% variation around that
    let jitter = (base_words / 3).max(1); // at least 1 word of wiggle

    for p in 0..pars {
        if p > 0 {
            out.push_str("\n\n");
        }

        let min_words = if base_words > jitter {
            base_words - jitter
        } else {
            1
        };
        let max_words = base_words + jitter;
        let range = max_words - min_words + 1;

        let para_words = min_words + rand_range(&mut seed, range);

        let para = generate_words(para_words, &mut seed);
        out.push_str(&para);
    }

    Ok(Value::Str(out))
}
