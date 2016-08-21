use super::context::BindgenContext;
use super::item::ItemId;
use clang;
use parse::{ClangItemParser, ClangSubItemParser, ParseError, ParseResult};

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

impl ClangSubItemParser for Module {
    fn parse(cursor: clang::Cursor, ctx: &mut BindgenContext) -> Result<ParseResult<Self>, ParseError> {
        use clangll::*;
        match cursor.kind() {
            CXCursor_Namespace => {
                // let namespace_name = match ctx.current_translation_unit().tokens(cursor) {
                //     None => None,
                //     Some(tokens) => {
                //         if tokens.len() <= 1 {
                //             None
                //         } else {
                //             match &*tokens[1].spelling {
                //                 "{" => None,
                //                 s => Some(s.to_owned()),
                //             }
                //         }
                //     }
                // };
                // TODO
                //
                // FIXME: Right now there's a loop in
                // clang::Cursor::is_toplevel, explicitly ignoring namespaces.
                // We should remove that loop once that lands.
                return Err(ParseError::Recurse)
            }
            _ => Err(ParseError::Continue)
        }
    }
}
