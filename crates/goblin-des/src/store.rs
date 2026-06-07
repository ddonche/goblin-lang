use std::collections::{BTreeSet, HashMap};
use indexmap::IndexMap;
use uuid::Uuid;
use crate::entity::{Entity, FieldValue};

/// Slot-based runtime handle. Cheap to copy, cheap to compare, safe to use
/// as HashMap/HashSet key. The generation prevents stale handles from
/// resolving to a recycled slot after the original entity was erased.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EntityHandle {
    pub slot: u32,
    pub generation: u32,
}

impl EntityHandle {
    pub const INVALID: EntityHandle = EntityHandle { slot: u32::MAX, generation: 0 };
}

impl std::fmt::Display for EntityHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "h{}g{}", self.slot, self.generation)
    }
}

/// A slot in the store. Carries the current generation so we can detect
/// stale handles without touching the entity itself.
enum Slot {
    Empty { generation: u32 },
    Occupied { entity: Entity },
}

impl Slot {
    fn generation(&self) -> u32 {
        match self {
            Slot::Empty { generation } => *generation,
            Slot::Occupied { entity } => entity.handle.generation,
        }
    }
}

/// The canonical entity store. One entity lives here; everything else is an
/// index that points into this store by EntityHandle.
pub struct EntityStore {
    slots: Vec<Slot>,
    free_list: Vec<u32>,
    handle_by_uuid: HashMap<Uuid, EntityHandle>,
    /// Variable name -> handle mapping (interpreter names entities by var name).
    handle_by_name: HashMap<String, EntityHandle>,
    name_by_handle: HashMap<EntityHandle, String>,
    /// Interpreter UUID string -> handle. The interpreter assigns its own UUID
    /// strings (stored as `fields["uuid"]`). This lets DES look up handles from
    /// those strings without scanning the slot array.
    handle_by_interp_uuid: HashMap<String, EntityHandle>,
}

impl EntityStore {
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            free_list: Vec::new(),
            handle_by_uuid: HashMap::new(),
            handle_by_name: HashMap::new(),
            name_by_handle: HashMap::new(),
            handle_by_interp_uuid: HashMap::new(),
        }
    }

    /// Create a new entity and return its handle.
    /// `fields` is used only to infer trait_fields and extract interp_uuid —
    /// it is NOT stored in the entity.
    pub fn create(
        &mut self,
        name: &str,
        class_name: String,
        fields: IndexMap<String, FieldValue>,
        raw_fields: BTreeSet<String>,
    ) -> EntityHandle {
        let (slot, generation) = if let Some(idx) = self.free_list.pop() {
            let next_gen = self.slots[idx as usize].generation() + 1;
            (idx, next_gen)
        } else {
            let idx = self.slots.len() as u32;
            (idx, 0)
        };

        let handle = EntityHandle { slot, generation };

        let interp_uuid = fields.get("uuid")
            .and_then(|v| if let crate::entity::FieldValue::Str(s) = v { Some(s.clone()) } else { None })
            .unwrap_or_default();

        let trait_fields = crate::entity::infer_trait_fields(&fields, &raw_fields);

        let entity = Entity::new(handle, class_name, interp_uuid.clone(), trait_fields, raw_fields);

        if !interp_uuid.is_empty() {
            self.handle_by_interp_uuid.insert(interp_uuid, handle);
        }

        self.handle_by_uuid.insert(entity.uuid, handle);
        self.handle_by_name.insert(name.to_string(), handle);
        self.name_by_handle.insert(handle, name.to_string());

        if slot as usize >= self.slots.len() {
            self.slots.push(Slot::Occupied { entity });
        } else {
            self.slots[slot as usize] = Slot::Occupied { entity };
        }

        handle
    }

    /// Look up a handle by the interpreter's UUID string (the `uuid` field value,
    /// not the DES-internal Uuid). Returns None if not registered.
    pub fn handle_for_interp_uuid(&self, uuid_str: &str) -> Option<EntityHandle> {
        self.handle_by_interp_uuid.get(uuid_str).copied()
    }

    /// Resolve a handle to an entity reference. Returns None if the handle
    /// is stale (generation mismatch) or the slot is empty.
    pub fn get(&self, handle: EntityHandle) -> Option<&Entity> {
        self.slots.get(handle.slot as usize).and_then(|slot| match slot {
            Slot::Occupied { entity } if entity.handle.generation == handle.generation => Some(entity),
            _ => None,
        })
    }

    pub fn get_mut(&mut self, handle: EntityHandle) -> Option<&mut Entity> {
        self.slots.get_mut(handle.slot as usize).and_then(|slot| match slot {
            Slot::Occupied { entity } if entity.handle.generation == handle.generation => Some(entity),
            _ => None,
        })
    }

    pub fn get_by_name(&self, name: &str) -> Option<&Entity> {
        let handle = self.handle_by_name.get(name)?;
        self.get(*handle)
    }

    pub fn get_by_name_mut(&mut self, name: &str) -> Option<&mut Entity> {
        let handle = *self.handle_by_name.get(name)?;
        self.get_mut(handle)
    }

    pub fn get_by_uuid(&self, uuid: &Uuid) -> Option<&Entity> {
        let handle = self.handle_by_uuid.get(uuid)?;
        self.get(*handle)
    }

    pub fn handle_for_name(&self, name: &str) -> Option<EntityHandle> {
        self.handle_by_name.get(name).copied()
    }

    pub fn handle_for_uuid(&self, uuid: &Uuid) -> Option<EntityHandle> {
        self.handle_by_uuid.get(uuid).copied()
    }

    pub fn name_for_handle(&self, handle: EntityHandle) -> Option<&str> {
        self.name_by_handle.get(&handle).map(|s| s.as_str())
    }

    /// Mark entity dead and free its slot for reuse. Removes name/uuid entries.
    /// The caller is responsible for removing the handle from all indexes before
    /// calling this.
    pub fn erase(&mut self, handle: EntityHandle) {
        if let Some(slot) = self.slots.get_mut(handle.slot as usize) {
            if let Slot::Occupied { entity } = slot {
                if entity.handle.generation == handle.generation {
                    let uuid = entity.uuid;
                    self.handle_by_interp_uuid.remove(&entity.interp_uuid);
                    let name = self.name_by_handle.remove(&handle).unwrap_or_default();
                    self.handle_by_uuid.remove(&uuid);
                    self.handle_by_name.remove(&name);
                    let slot_gen = entity.handle.generation;
                    *slot = Slot::Empty { generation: slot_gen };
                    self.free_list.push(handle.slot);
                }
            }
        }
    }

    /// Rename a handle's associated variable name (used during transitions where
    /// the entity keeps its handle but the programmer renames the variable).
    pub fn rename(&mut self, handle: EntityHandle, new_name: &str) {
        if let Some(old_name) = self.name_by_handle.get(&handle).cloned() {
            self.handle_by_name.remove(&old_name);
        }
        self.handle_by_name.insert(new_name.to_string(), handle);
        self.name_by_handle.insert(handle, new_name.to_string());
    }

    /// Iterate over all live entity handles.
    pub fn live_handles(&self) -> impl Iterator<Item = EntityHandle> + '_ {
        self.slots.iter().filter_map(|slot| match slot {
            Slot::Occupied { entity } if entity.alive => Some(entity.handle),
            _ => None,
        })
    }

    /// Iterate over all live entities.
    pub fn live_entities(&self) -> impl Iterator<Item = &Entity> {
        self.slots.iter().filter_map(|slot| match slot {
            Slot::Occupied { entity } if entity.alive => Some(entity),
            _ => None,
        })
    }

    pub fn live_count(&self) -> usize {
        self.slots.iter().filter(|s| matches!(s, Slot::Occupied { entity } if entity.alive)).count()
    }
}

impl Default for EntityStore {
    fn default() -> Self {
        Self::new()
    }
}
