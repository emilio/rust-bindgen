use super::item_kind::ItemKind;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};

/// A single identifier for an item.
///
/// TODO: Build stronger abstractions on top of this, like TypeId(ItemId), ...
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(usize);

pub static NEXT_ITEM_ID: AtomicUsize = ATOMIC_USIZE_INIT;

impl ItemId {
    pub fn next() -> Self {
        ItemId(NEXT_ITEM_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug)]
pub struct Item {
    /// This item's id.
    id: ItemId,
    /// A given item's module id. A module's module id is itself.
    module_id: ItemId,
    /// A doc comment over the item, if any.
    comment: Option<String>,
    /// The item kind.
    kind: ItemKind,
}

impl Item {
    pub fn new(id: ItemId,
               comment: Option<String>,
               module_id: ItemId,
               kind: ItemKind) -> Self {
        debug_assert!(id != module_id || kind.is_module());
        Item {
            id: id,
            module_id: module_id,
            comment: comment,
            kind: kind,
        }
    }

    pub fn id(&self) -> ItemId {
        self.id
    }

    pub fn module_id(&self) -> ItemId {
        self.module_id
    }

    pub fn doc_comment(&self) -> Option<&str> {
        self.comment.as_ref().map(|c| &**c)
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }
}
