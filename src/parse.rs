use clang;
use ir::item::ItemId;
use ir::context::BindgenContext;

pub enum ParseError {
    Recurse,
    Continue,
}

pub trait ClangSubItemParser : Sized {
    /// The fact that is a reference guarantees it's holded by the context, and
    /// allow returning already existing types.
    fn parse(cursor: clang::Cursor, context: &mut BindgenContext) -> Result<Self, ParseError> {
        // FIXME: All types should have it implemented properly, this is just
        // for testing.
        None
    }
}

pub trait ClangItemParser: Sized {
    /// The fact that is a reference guarantees it's holded by the context, and
    /// allow returning already existing types.
    fn parse(cursor: clang::Cursor, context: &mut BindgenContext) -> Result<ItemId, ParseError>;

    fn from_ty(ty: &clang::Type, context: &mut BindgenContext) -> Result<ItemId, ParseError>;
}

macro_rules! stub {
    ($ty:ty) => {
        impl ClangSubItemParser for $ty {}
    }
}

stub!(::ir::var::Var);
stub!(::ir::function::Function);
