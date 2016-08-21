use super::item::ItemId;
use syntax::abi;

/// A function declaration , with a signature, arguments, and argument names.
///
/// The argument names vector must be the same length as the ones in the
/// signature.
#[derive(Debug)]
pub struct Function {
    name: String,
    /// The id pointing to the current function signature.
    signature: ItemId,
    /// The arguments names for this function.
    arg_names: Vec<String>,
}

impl Function {
    pub fn new(name: String, sig: ItemId, arg_names: Vec<String>) -> Self {
        Function {
            name: name,
            signature: sig,
            arg_names: arg_names,
        }
    }
}

/// A function signature.
#[derive(Debug)]
pub struct FunctionSig {
    /// The return type of the function.
    return_type: ItemId,
    /// The type of the arguments.
    argument_types: Vec<ItemId>,
    /// Whether this function is variadic.
    is_variadic: bool,
    /// Whether this function is considered "safe".
    /// XXX: Probably all should be unsafe.
    is_safe: bool,
    /// The abi of this function.
    abi: abi::Abi,
}

impl FunctionSig {
    pub fn new(return_type: ItemId,
               argument_types: Vec<ItemId>,
               is_variadic: bool,
               is_safe: bool,
               abi: abi::Abi) -> Self {
        FunctionSig {
            return_type: return_type,
            argument_types: argument_types,
            is_variadic: is_variadic,
            is_safe: false,
            abi: abi,
        }
    }
}
