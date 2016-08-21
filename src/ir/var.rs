use super::item::ItemId;

#[derive(Clone, PartialEq)]
pub struct VarInfo {
    /// The name of the variable.
    name: String,
    /// The mangled name of the variable.
    mangled_name: Option<String>,
    /// The type of the variable.
    ty: ItemId,
    // TODO: support non-integer constants
    /// The integer value of the variable.
    val: Option<i64>,
    /// Whether this variable is const.
    is_const: bool,
    /// Whether this variable is static.
    is_static: bool,
}

impl VarInfo {
    pub fn new(name: String,
               mangled: Option<String>,
               ty: ItemId,
               val: Option<i64>,
               is_const: bool,
               is_static: bool) -> VarInfo {
        VarInfo {
            name: name,
            mangled_name: mangled,
            ty: ty,
            val: val,
            is_const: is_const,
            is_static: is_static,
        }
    }
}
