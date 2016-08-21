use super::item::{Item, ItemId};
use super::context::BindgenContext;
use syntax::abi;
use clang;
use clangll::Enum_CXCallingConv;
use parse::ClangItemParser;

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

fn get_abi(cc: Enum_CXCallingConv) -> abi::Abi {
    use clangll::*;
    match cc {
        CXCallingConv_Default => abi::Abi::C,
        CXCallingConv_C => abi::Abi::C,
        CXCallingConv_X86StdCall => abi::Abi::Stdcall,
        CXCallingConv_X86FastCall => abi::Abi::Fastcall,
        CXCallingConv_AAPCS => abi::Abi::Aapcs,
        CXCallingConv_X86_64Win64 => abi::Abi::Win64,
        other => panic!("unsupported calling convention: {}", other),
    }
}


impl FunctionSig {
    pub fn new(return_type: ItemId,
               argument_types: Vec<ItemId>,
               is_variadic: bool,
               abi: abi::Abi) -> Self {
        FunctionSig {
            return_type: return_type,
            argument_types: argument_types,
            is_variadic: is_variadic,
            is_safe: false,
            abi: abi,
        }
    }

    pub fn from_ty(ty: &clang::Type,
                 cursor: &clang::Cursor,
                 ctx: &mut BindgenContext) -> Option<Self> {
        use clangll::*;
        let args: Vec<_> = match cursor.kind() {
            CXCursor_FunctionDecl | CXCursor_CXXMethod => {
                // For CXCursor_FunctionDecl, cursor.args() is the reliable way to
                // get parameter names and types.
                cursor.args().iter().map(|arg| {
                    let arg_ty = arg.cur_type();
                    Item::from_ty(&arg_ty, ctx)
                            .expect("Unable to resolve argument type")
                }).collect()
            }
            // TODO: Old parser fell back to walk the ast.
            _ => return None,
        };

        // TODO: gracefully return error here.
        let ret = Item::from_ty(&ty.ret_type(), ctx).expect("No return type");
        let abi = get_abi(ty.call_conv());

        Some(Self::new(ret, args, ty.is_variadic(), abi))
    }
}
