use super::ty::{Type, TypeKind, FloatKind};
use super::item::{Item, ItemId};
use super::item_kind::ItemKind;
use super::int::IntKind;
use super::module::Module;
use clang::{self, Cursor};
use std::collections::HashMap;

/// A context used during parsing and generation of structs.
#[derive(Debug)]
pub struct BindgenContext {
    /// The map of all the items parsed so far.
    items: HashMap<ItemId, Item>,

    /// Clang cursor to item map. This is needed to be able to associate types
    /// with item ids during parsing.
    types: HashMap<Cursor, ItemId>,

    /// The type id for the void type.

    /// The root module, this is guaranteed to be an item of kind Module.
    root_module: ItemId,

    /// Current module being traversed.
    current_module: ItemId,
}

impl BindgenContext {
    pub fn new() -> Self {
        let root_module = Self::build_root_module();
        let mut me = BindgenContext {
            items: Default::default(),
            types: Default::default(),
            root_module: root_module.id(),
            current_module: root_module.id(),
        };

        me.add_item(root_module, None);

        me
    }

    pub fn add_item(&mut self, item: Item, declaration: Option<Cursor>) {
        debug_assert!(item.kind().is_type() == declaration.is_some(),
                      "Adding a type without declaration?");
        let id = item.id();
        let is_type = item.kind().is_type();
        let old_item = self.items.insert(id, item);
        assert!(old_item.is_none(), "Inserted type twice?");

        if is_type {
            self.types.insert(declaration.unwrap(), id);
        }
    }

    fn build_root_module() -> Item {
        let module = Module::new(None, None);
        let id = ItemId::next();
        Item::new(id, None, id, ItemKind::Module(module))
    }

    fn resolve_type(&self, type_id: ItemId) -> &Type {
        self.items.get(&type_id).unwrap().kind().expect_type()
    }

    pub fn current_module(&self) -> ItemId {
        self.current_module
    }

    /// Looks up for an already resolved type, either because it's builtin, or
    /// because we already have it in the map.
    pub fn builtin_or_resolved_ty(&mut self, ty: &clang::Type) -> Option<ItemId> {
        let declaration = ty.declaration();

        // First lookup to see if we already have it resolved.
        if let Some(ty) = self.types.get(&declaration) {
            // TODO: we might want to update the layout?
            return Some(*ty);
        }

        // Else, build it.
        self.build_builtin_ty(ty, declaration)
    }

    fn build_builtin_ty(&mut self,
                        ty: &clang::Type,
                        declaration: Cursor) -> Option<ItemId> {
        use clangll::*;
        let type_kind = match ty.kind() {
            CXType_NullPtr => TypeKind::NullPtr,
            CXType_Void => TypeKind::Void,
            CXType_Int => TypeKind::Int(IntKind::Int),
            CXType_UInt => TypeKind::Int(IntKind::UInt),
            CXType_SChar |
            CXType_Char_S => TypeKind::Int(IntKind::Char),
            CXType_UChar |
            CXType_Char_U => TypeKind::Int(IntKind::UChar),
            CXType_Short => TypeKind::Int(IntKind::Short),
            // TODO: IntKind::Char16?
            CXType_UShort |
            CXType_Char16 => TypeKind::Int(IntKind::UShort),
            CXType_Long => TypeKind::Int(IntKind::Long),
            CXType_ULong => TypeKind::Int(IntKind::ULong),
            CXType_LongLong => TypeKind::Int(IntKind::LongLong),
            CXType_ULongLong => TypeKind::Int(IntKind::ULongLong),
            CXType_Float => TypeKind::Float(FloatKind::Float),
            CXType_Double => TypeKind::Float(FloatKind::Double),
            CXType_LongDouble => TypeKind::Float(FloatKind::LongDouble),
            _ => return None,
        };

        let spelling = ty.spelling();
        let layout = ty.fallible_layout().ok();
        let ty = Type::new(Some(spelling), layout, type_kind);
        let id = ItemId::next();
        let item = Item::new(id, None, self.root_module, ItemKind::Type(ty));
        self.add_item(item, Some(declaration));
        Some(id)
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

