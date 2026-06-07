use std::collections::{BTreeSet, HashMap};
use indexmap::IndexMap;
use uuid::Uuid;
use crate::store::EntityHandle;

/// A field value in DES. Kept minimal — the interpreter owns full Value; we
/// mirror only what DES needs to maintain indexes and run tick_db.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
}

impl FieldValue {
    /// Returns the f64 if this is a Float, else None.
    pub fn as_float(&self) -> Option<f64> {
        match self {
            FieldValue::Float(v) => Some(*v),
            FieldValue::Int(v) => Some(*v as f64),
            _ => None,
        }
    }

    /// True if this value qualifies as a trait field (numeric, 0..1).
    pub fn is_trait_value(&self) -> bool {
        match self {
            FieldValue::Float(v) => *v >= 0.0 && *v <= 1.0,
            _ => false,
        }
    }
}

/// The canonical entity record stored in EntityStore.
#[derive(Debug, Clone)]
pub struct Entity {
    /// Stable, saveable identity. Never changes across the entity's lifetime.
    pub uuid: Uuid,

    /// Fast runtime slot identity. Used by all indexes.
    pub handle: EntityHandle,

    /// Current class name.
    pub class_name: String,

    /// Current committed field state (what everyone reads from during a tick).
    pub fields: IndexMap<String, FieldValue>,

    /// Pending field state (where writes land during a tick).
    /// Swapped with `fields` at end of tick.
    pub pending: IndexMap<String, FieldValue>,

    /// Field names that are raw (prefixed with `~`). Never clamped, never
    /// treated as traits regardless of value.
    pub raw_fields: BTreeSet<String>,

    /// Field names inferred as traits (numeric 0..1, not raw).
    /// Recomputed whenever fields change type or raw status changes.
    pub trait_fields: BTreeSet<String>,

    /// Whether this entity is alive. Dead entities stay in the store until
    /// the next compaction but are excluded from all index queries.
    pub alive: bool,

    /// Arbitrary metadata the host system (interpreter) may attach.
    pub tags: HashMap<String, String>,
}

impl Entity {
    pub fn new(handle: EntityHandle, class_name: String, fields: IndexMap<String, FieldValue>, raw_fields: BTreeSet<String>) -> Self {
        let trait_fields = infer_trait_fields(&fields, &raw_fields);
        let pending = fields.clone();
        Self {
            uuid: Uuid::new_v4(),
            handle,
            class_name,
            fields,
            pending,
            raw_fields,
            trait_fields,
            alive: true,
            tags: HashMap::new(),
        }
    }

    /// Read a field from committed state (used during tick evaluation).
    pub fn get(&self, field: &str) -> Option<&FieldValue> {
        self.fields.get(field)
    }

    /// Write a field to pending state (batched until tick swap).
    pub fn set_pending(&mut self, field: &str, value: FieldValue) {
        if self.trait_fields.contains(field) {
            // Clamp traits to 0..1 on write.
            let v = match &value {
                FieldValue::Float(f) => FieldValue::Float(f.clamp(0.0, 1.0)),
                FieldValue::Int(i) => FieldValue::Float((*i as f64).clamp(0.0, 1.0)),
                other => other.clone(),
            };
            self.pending.insert(field.to_string(), v);
        } else {
            self.pending.insert(field.to_string(), value);
        }
    }

    /// Swap pending into committed. Called by TickRunner at end of tick.
    /// Returns the set of fields whose committed value actually changed,
    /// so indexes can update only what changed.
    pub fn swap(&mut self) -> Vec<String> {
        let mut changed = Vec::new();
        for (k, v) in &self.pending {
            if self.fields.get(k) != Some(v) {
                changed.push(k.clone());
                self.fields.insert(k.clone(), v.clone());
            }
        }
        changed
    }

    /// Recompute trait_fields from current committed state.
    pub fn refresh_traits(&mut self) {
        self.trait_fields = infer_trait_fields(&self.fields, &self.raw_fields);
    }
}

fn infer_trait_fields(fields: &IndexMap<String, FieldValue>, raw: &BTreeSet<String>) -> BTreeSet<String> {
    fields
        .iter()
        .filter(|(k, v)| !raw.contains(*k) && v.is_trait_value())
        .map(|(k, _)| k.clone())
        .collect()
}
