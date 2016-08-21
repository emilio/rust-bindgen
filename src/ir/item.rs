use super::context::BindgenContext;
use super::item_kind::ItemKind;
use super::ty::Type;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use parse::{ClangItemParser, ClangSubItemParser, ParseError};
use clang;

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

impl ClangItemParser for Item {
    fn parse(cursor: clang::Cursor, context: &mut BindgenContext) -> Result<ItemId, ParseError> {
        use ir::function::Function;
        use ir::module::Module;
        use ir::ty::Type;
        use ir::var::Var;

        let cursor = cursor.canonical();
        let comment = cursor.raw_comment();
        let comment = if comment.is_empty() { None } else { Some(comment) };
        let current_module = context.current_module();

        macro_rules! try_parse {
            ($what:ident, $decl:expr) => {
                match $what::parse(cursor, context) {
                    Ok(item) => {
                        let id = ItemId::next();
                        context.add_item(Item::new(id, comment, id,
                                                   ItemKind::$what(item)),
                                         $decl);
                        return Ok(id);
                    }
                    Err(ParseError::Recurse) => return Err(ParseError::Recurse),
                    Err(ParseError::Continue) => {},
                }
            }
        }

        try_parse!(Module, None);
        try_parse!(Type, Some(cursor));
        try_parse!(Function, None);
        try_parse!(Var, None);

        warn!("Unhandled cursor kind: {}", ::clang::kind_to_str(cursor.kind()));
        Err(ParseError::Continue)
    }

    fn from_ty(ty: &clang::Type, context: &mut BindgenContext) -> Option<ItemId> {
        use ir::ty::TypeResult;
        let comment = ty.declaration().raw_comment();
        let comment = if comment.is_empty() { None } else { Some(comment) };
        if let Some(ty) = Type::from_clang_ty(ty, context) {
            let current_module = context.current_module();
            match ty {
                TypeResult::AlreadyResolved(item_id) => return Some(item_id),
                TypeResult::New(item, declaration) => {
                    let id = ItemId::next();
                    context.add_item(Item::new(id, comment,
                                               current_module,
                                               ItemKind::Type(item)),
                                     Some(declaration));
                    return Some(id);
                }
            }
        }

        None
    }
}
