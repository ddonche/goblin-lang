use std::collections::{BTreeSet, HashMap};
use uuid::Uuid;
use crate::store::EntityHandle;

/// A field value in DES. Kept minimal — the interpreter owns full Value; we
/// mirror only what DES needs for index maintenance and tick_db writes.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
}

impl FieldValue {
    pub fn as_float(&self) -> Option<f64> {
        match self {
            FieldValue::Float(v) => Some(*v),
            FieldValue::Int(v) => Some(*v as f64),
            _ => None,
        }
    }

    pub fn is_trait_value(&self) -> bool {
        matches!(self, FieldValue::Float(v) if *v >= 0.0 && *v <= 1.0)
    }
}

/// Lightweight entity record. DES does NOT duplicate the committed field state —
/// that lives exclusively in the interpreter's object_store. Entity carries only
/// what the index and double-buffer system needs:
///
///   - identity (uuid, handle, class)
///   - trait field set (for clamping during pending writes)
///   - sparse pending buffer (only fields written during the current tick)
///   - alive flag
///
/// Memory cost per entity: ~3 BTreeSet/HashMap headers + trait set entries.
/// No full field copy. No pending clone at creation.
#[derive(Debug, Clone)]
pub struct Entity {
    /// DES-internal stable UUID.
    pub uuid: Uuid,

    /// The interpreter's UUID string, mirrored here for index lookups.
    pub interp_uuid: String,

    /// Fast runtime slot identity. Used by all indexes.
    pub handle: EntityHandle,

    /// Current class name.
    pub class_name: String,

    /// Field names inferred as traits (numeric 0..1, not raw) at creation.
    /// Used to clamp writes in set_pending. Re-set on mutate transition.
    pub trait_fields: BTreeSet<String>,

    /// Field names that are raw (prefixed with `~`). Never clamped.
    pub raw_fields: BTreeSet<String>,

    /// Sparse pending buffer. Only populated during a tick when set_pending()
    /// is called. Cleared after TickRunner flushes changes to object_store.
    pub pending: HashMap<String, FieldValue>,

    /// Whether this entity is alive.
    pub alive: bool,
}

impl Entity {
    /// Create a new entity. `initial_fields` is used only to infer trait_fields
    /// and extract interp_uuid — it is NOT stored in the entity.
    pub fn new(
        handle: EntityHandle,
        class_name: String,
        interp_uuid: String,
        trait_fields: BTreeSet<String>,
        raw_fields: BTreeSet<String>,
    ) -> Self {
        Self {
            uuid: Uuid::new_v4(),
            interp_uuid,
            handle,
            class_name,
            trait_fields,
            raw_fields,
            pending: HashMap::new(), // sparse — starts empty every tick
            alive: true,
        }
    }

    /// Write a field to the sparse pending buffer.
    /// Trait fields are clamped to 0..1 on write.
    pub fn set_pending(&mut self, field: &str, value: FieldValue) {
        let v = if self.trait_fields.contains(field) {
            match value {
                FieldValue::Float(f) => FieldValue::Float(f.clamp(0.0, 1.0)),
                FieldValue::Int(i) => FieldValue::Float((i as f64).clamp(0.0, 1.0)),
                other => other,
            }
        } else {
            value
        };
        self.pending.insert(field.to_string(), v);
    }

    /// Drain the pending buffer and return its contents.
    /// Called by TickRunner — the caller writes changes to object_store.
    pub fn drain_pending(&mut self) -> HashMap<String, FieldValue> {
        std::mem::take(&mut self.pending)
    }

    /// True if there are any pending writes this tick.
    pub fn has_pending(&self) -> bool {
        !self.pending.is_empty()
    }
}

/// Infer trait field names from a field map and raw-field set.
/// Called at registration time; result is stored on Entity.
pub fn infer_trait_fields(
    fields: &indexmap::IndexMap<String, FieldValue>,
    raw: &BTreeSet<String>,
) -> BTreeSet<String> {
    fields
        .iter()
        .filter(|(k, v)| !raw.contains(*k) && v.is_trait_value())
        .map(|(k, _)| k.clone())
        .collect()
}
