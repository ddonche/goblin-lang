use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum YallValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Array(Vec<YallValue>),
    Map(BTreeMap<String, YallValue>),
}

impl YallValue {
    pub fn null() -> Self { YallValue::Null }
    pub fn bool(b: bool) -> Self { YallValue::Bool(b) }
    pub fn int(i: i64) -> Self { YallValue::Int(i) }
    pub fn float(f: f64) -> Self { YallValue::Float(f) }
    pub fn str<S: Into<String>>(s: S) -> Self { YallValue::Str(s.into()) }
    pub fn array(v: Vec<YallValue>) -> Self { YallValue::Array(v) }
    pub fn map(m: BTreeMap<String, YallValue>) -> Self { YallValue::Map(m) }

    pub fn is_null(&self) -> bool { matches!(self, YallValue::Null) }
    pub fn is_map(&self)  -> bool { matches!(self, YallValue::Map(_)) }
    pub fn is_array(&self)-> bool { matches!(self, YallValue::Array(_)) }

    pub fn as_map(&self) -> Option<&BTreeMap<String, YallValue>> {
        if let YallValue::Map(ref m) = self { Some(m) } else { None }
    }

    pub fn as_array(&self) -> Option<&Vec<YallValue>> {
        if let YallValue::Array(ref a) = self { Some(a) } else { None }
    }

    pub fn as_str(&self) -> Option<&str> {
        if let YallValue::Str(ref s) = self { Some(s) } else { None }
    }
}

impl fmt::Display for YallValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            YallValue::Null => write!(f, "null"),
            YallValue::Bool(b) => write!(f, "{}", b),
            YallValue::Int(i) => write!(f, "{}", i),
            YallValue::Float(fl) => write!(f, "{}", fl),
            YallValue::Str(s) => write!(f, "{:?}", s), // quoted debug style
            YallValue::Array(arr) => {
                write!(f, "[")?;
                let mut first = true;
                for v in arr {
                    if !first { write!(f, ", ")?; }
                    first = false;
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            YallValue::Map(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in map {
                    if !first { write!(f, ", ")?; }
                    first = false;
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
        }
    }
}
