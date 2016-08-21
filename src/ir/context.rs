use super::ty::{Type, TypeKind};
use super::item::{Item, ItemId};
use super::item_kind::ItemKind;
use super::int::IntKind;
use super::module::Module;
use clang::Cursor;
use std::collections::HashMap;

/// A context used during parsing and generation of structs.
#[derive(Debug)]
pub struct BindgenContext {
    /// The map of all the items parsed so far.
    items: HashMap<ItemId, Item>,

    /// The builtin type layouts for the current integer types.
    ///
    /// The map is filled on context creation, and the layouts are recorded on
    /// the fly.
    builtin_int_types: HashMap<IntKind, ItemId>,
    /// The builtin void type id.
    builtin_void_id: ItemId,

    /// Clang cursor to item map. This is needed to be able to associate types
    /// with item ids during parsing.
    clang_cursor_map: HashMap<Cursor, ItemId>,

    /// The type id for the void type.

    /// The root module, this is guaranteed to be an item of kind Module.
    root_module: ItemId,

    /// Current module being traversed.
    current_module: ItemId,
}

impl BindgenContext {
    pub fn new() -> Self {
        let root_module = Self::build_root_module();
        let void_id = ItemId::next();
        let mut me = BindgenContext {
            items: Default::default(),
            builtin_int_types: Default::default(),
            builtin_void_id: void_id,
            clang_cursor_map: Default::default(),
            root_module: root_module.id(),
            current_module: root_module.id(),
        };

        me.add_item(root_module);
        me.fill_builtin_types(void_id);

        me
    }

    fn add_item(&mut self, item: Item) {
        let old_item: Option<_> = self.items.insert(item.id(), item);
        assert!(old_item.is_none(), "Inserted type twice?");
    }

    fn build_root_module() -> Item {
        let module = Module::new(None, None);
        let id = ItemId::next();
        Item::new(id, None, id, ItemKind::Module(module))
    }

    fn fill_builtin_types(&mut self, void_id: ItemId) {
        debug_assert!(self.current_module == self.root_module);

        IntKind::each(|kind| {
            let id = ItemId::next();
            // TODO: names!
            let item_kind = ItemKind::Type(Type::new(None, None, None, TypeKind::Int(kind)));
            let item = Item::new(id, None, self.current_module, item_kind);
            self.builtin_int_types.insert(kind, id);
            self.add_item(item);
        });

        let void_kind = ItemKind::Type(Type::new(Some("void".into()), None, None, TypeKind::Void));
        let item = Item::new(void_id, None, self.current_module, void_kind);
        self.add_item(item);
    }

    fn resolve_type(&self, type_id: ItemId) -> &Type {
        self.items.get(&type_id).unwrap().kind().expect_type()
    }
}

/// A type that only exposes the resolve_type operation to its consumers.
pub struct TypeResolver<'a>(&'a BindgenContext);

impl<'a> TypeResolver<'a> {
    pub fn new(ctx: &'a BindgenContext) -> Self {
        TypeResolver(ctx)
    }

    pub fn resolve_type(&self, type_id: ItemId) -> &Type {
        self.0.resolve_type(type_id)
    }
}

