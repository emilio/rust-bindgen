use super::context::BindgenContext;
use super::item_kind::ItemKind;
use super::ty::{Type, TypeKind};
use std::fmt;
use std::sync::atomic::{AtomicUsize, ATOMIC_USIZE_INIT, Ordering};
use parse::{ClangItemParser, ClangSubItemParser, ParseError, ParseResult};
use clang;

/// A single identifier for an item.
///
/// TODO: Build stronger abstractions on top of this, like TypeId(ItemId), ...
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(usize);

impl fmt::Display for ItemId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "_bindgen_id_"));
        self.0.fmt(fmt)
    }
}

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
    /// An item's parent id. This will most likely be a class where this item
    /// was declared, or a module, etc.
    ///
    /// All the items have a parent, except the root module, in which case the
    /// parent id is its own id.
    parent_id: ItemId,
    /// A doc comment over the item, if any.
    comment: Option<String>,
    /// The item kind.
    kind: ItemKind,
}

impl Item {
    pub fn new(id: ItemId,
               comment: Option<String>,
               parent_id: ItemId,
               kind: ItemKind) -> Self {
        debug_assert!(id != parent_id || kind.is_module());
        Item {
            id: id,
            parent_id: parent_id,
            comment: comment,
            kind: kind,
        }
    }

    pub fn id(&self) -> ItemId {
        self.id
    }

    pub fn parent_id(&self) -> ItemId {
        self.parent_id
    }

    pub fn comment(&self) -> Option<&str> {
        self.comment.as_ref().map(|c| &**c)
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn is_toplevel(&self, ctx: &BindgenContext) -> bool {
        self.parent_id == ctx.root_module()
    }

    pub fn expect_type(&self) -> &Type {
        self.kind().expect_type()
    }
}

impl ClangItemParser for Item {
    fn builtin_type(kind: TypeKind, is_const: bool, ctx: &mut BindgenContext) -> ItemId {
        match kind {
            TypeKind::Int(..) |
            TypeKind::Float(..) => {},
            _ => panic!("Unsupported builtin type"),
        }

        let ty = Type::new(None, None, kind, is_const);
        let id = ItemId::next();
        let module = ctx.root_module();
        ctx.add_item(Item::new(id, None, module, ItemKind::Type(ty)),
                     None, None);
        id
    }


    fn parse(cursor: clang::Cursor,
             parent_id: Option<ItemId>,
             context: &mut BindgenContext) -> Result<ItemId, ParseError> {
        use ir::function::Function;
        use ir::module::Module;
        use ir::ty::Type;
        use ir::var::Var;
        use clangll::CXCursor_Namespace;

        if !cursor.is_valid() {
            return Err(ParseError::Continue);
        }

        let comment = cursor.raw_comment();

        // FIXME: The current_module logic is not really accurate. We should be
        // able to index modules by their Cursor, and locate the proper module
        // for a given item.
        //
        // We don't support modules properly though, so there's no rush for
        // this.
        let current_module = context.current_module();
        macro_rules! try_parse {
            ($what:ident) => {
                match $what::parse(cursor, context) {
                    Ok(ParseResult::New(item, declaration)) => {
                        let id = ItemId::next();
                        context.add_item(Item::new(id, comment,
                                                   parent_id.unwrap_or(current_module),
                                                   ItemKind::$what(item)),
                                         declaration,
                                         Some(cursor));
                        return Ok(id);
                    }
                    Ok(ParseResult::AlreadyResolved(id)) => {
                        return Ok(id);
                    }
                    Err(ParseError::Recurse) => return Err(ParseError::Recurse),
                    Err(ParseError::Continue) => {},
                }
            }
        }

        try_parse!(Module);

        // NOTE: Is extremely important to parse functions and vars **before**
        // types.  Otherwise we can parse a function declaration as a type
        // (which is legal), and lose functions to generate.
        //
        // In general, I'm not totally confident this split between
        // ItemKind::Function and TypeKind::FunctionSig is totally worth it, but
        // I guess we can try.
        try_parse!(Function);
        try_parse!(Var);

        // Types are sort of special, so to avoid parsing template classes
        // twice, handle them separately.
        {
            let definition = cursor.definition();
            let applicable_cursor = if definition.is_valid() {
                definition
            } else {
                cursor
            };
            match Self::from_ty(&applicable_cursor.cur_type(),
                                Some(applicable_cursor), parent_id, context)
            {
                Ok(ty) => return Ok(ty),
                Err(ParseError::Recurse) => return Err(ParseError::Recurse),
                Err(ParseError::Continue) => {},
            }
        }

        warn!("Unhandled cursor kind: {}", ::clang::kind_to_str(cursor.kind()));
        Err(ParseError::Continue)
    }

    fn from_ty(ty: &clang::Type,
               location: Option<clang::Cursor>,
               parent_id: Option<ItemId>,
               context: &mut BindgenContext) -> Result<ItemId, ParseError> {
        use clangll::*;

        let decl = ty.declaration();
        let definition = decl.definition();
        let decl = if definition.is_valid() {
            definition
        } else {
            decl
        };

        let comment =
            definition.raw_comment()
                      .or_else(|| location.as_ref().and_then(|l| l.raw_comment()));

        if let Some(ty) = context.builtin_or_resolved_ty(parent_id, ty, location) {
            return Ok(ty);
        }

        // First, check we're not recursing.
        let mut valid_decl = decl.kind() != CXCursor_NoDeclFound;
        let declaration_to_look_for = if valid_decl {
            decl.canonical()
        } else if location.is_some() && location.unwrap().kind() == CXCursor_ClassTemplate {
            valid_decl = true;
            location.unwrap()
        } else {
            decl
        };

        if valid_decl {
            if let Some(&(_, item_id)) = context.currently_parsed_types.iter().find(|&&(d, _)| d == declaration_to_look_for) {
                debug!("Avoiding recursion parsing type: {:?}", ty);
                return Ok(item_id);
            }
        }

        // NB: This is useless in the AlreadyResolved case, so yeah, we lose an
        // id, but it's not really important...
        let id = ItemId::next();
        let current_module = context.current_module();
        if valid_decl {
            context.currently_parsed_types.push((declaration_to_look_for, id));
        }

        let mut result = Type::from_clang_ty(id, ty, location, parent_id, context);
        let ret = match result {
            Ok(ParseResult::AlreadyResolved(ty)) => Ok(ty),
            Ok(ParseResult::New(item, declaration)) => {
                context.add_item(Item::new(id, comment,
                                           parent_id.unwrap_or(current_module),
                                           ItemKind::Type(item)),
                                 declaration,
                                 location);
                Ok(id)
            }
            Err(ParseError::Continue) => Err(ParseError::Continue),
            Err(ParseError::Recurse) => {
                debug!("Item::from_ty recursing in the ast");
                let mut result = Err(ParseError::Recurse);
                if let Some(ref location) = location {
                    // Need to pop here, otherwise we'll get stuck.
                    //
                    // TODO: Find a nicer interface, really. Also, the
                    // declaration_to_look_for suspiciously shares a lot of
                    // logic with ir::context, so we should refactor that.
                    if valid_decl {
                        let (popped_decl, _) = context.currently_parsed_types.pop().unwrap();
                        assert_eq!(popped_decl, declaration_to_look_for);
                    }

                    location.visit(|cur, other| {
                        use clangll::*;
                        result = Item::from_ty(ty, Some(*cur), parent_id, context);
                        match result {
                            Ok(..) => CXChildVisit_Break,
                            Err(ParseError::Recurse) => CXChildVisit_Recurse,
                            Err(ParseError::Continue) => CXChildVisit_Continue,
                        }
                    });

                    if valid_decl {
                        context.currently_parsed_types.push((declaration_to_look_for, id));
                    }
                }
                // If we have recursed into the AST all we know, and we still
                // haven't found what we've got, let's
                // just make a named type.
                //
                // This is what happens with some template members, for example.
                //
                // FIXME: Maybe we should restrict this to things with parent?
                // It's harmless, but if we restrict that, then
                // tests/headers/nsStyleAutoArray.hpp crashes.
                if let Err(ParseError::Recurse) = result {
                    Ok(Self::named_type_with_id(id, ty.spelling(),
                                                parent_id.unwrap_or(context.current_module()),
                                                context))
                } else {
                    result
                }
            }
        };

        if valid_decl {
            let (popped_decl, _) = context.currently_parsed_types.pop().unwrap();
            assert_eq!(popped_decl, declaration_to_look_for);
        }

        ret
    }

    /// A named type is a template parameter, e.g., the "T" in Foo<T>. They're
    /// always local so it's the only exception when there's no declaration for
    /// a type.
    ///
    /// It must have an id, and must not be the current module id. Ideally we
    /// could assert the parent id is a Comp(..) type, but that info isn't
    /// available yet.
    fn named_type_with_id<S>(id: ItemId,
                             name: S,
                             parent_id: ItemId,
                             context: &mut BindgenContext) -> ItemId
        where S: Into<String>
    {
        let name = name.into();

        context.add_item(Item::new(id, None, parent_id,
                                   ItemKind::Type(Type::named(name))),
                         None,
                         None);

        id
    }

    fn named_type<S>(name: S, parent_id: ItemId, context: &mut BindgenContext) -> ItemId
        where S: Into<String>
    {
        Self::named_type_with_id(ItemId::next(), name, parent_id, context)
    }
}
