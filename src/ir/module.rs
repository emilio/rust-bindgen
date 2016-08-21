use super::item::ItemId;

/// A module, as in, a C++ namespace.
#[derive(Clone, Debug)]
pub struct Module {
    /// The name of the module, or none if it's the root or an anonymous
    /// namespace.
    name: Option<String>,
    /// The parent module of this.
    parent_id: Option<ItemId>,
    /// The children of this module, just here for convenience.
    children_ids: Vec<ItemId>,
}

impl Module {
    pub fn new(name: Option<String>, parent_id: Option<ItemId>) -> Self {
        Module {
            name: name,
            parent_id: None,
            children_ids: vec![],
        }
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|s| &**s)
    }

    pub fn parent(&self) -> Option<ItemId> {
        self.parent_id
    }
}
