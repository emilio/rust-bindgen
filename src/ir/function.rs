use super::item::{Item, ItemId};
use super::context::BindgenContext;
use syntax::abi;
use clang;
use clangll::Enum_CXCallingConv;
use parse::{ClangItemParser, ClangSubItemParser, ParseError, ParseResult};

/// A function declaration , with a signature, arguments, and argument names.
///
/// The argument names vector must be the same length as the ones in the
/// signature.
#[derive(Debug)]
pub struct Function {
    name: String,
    /// The mangled name, that is, the symbol.
    mangled_name: Option<String>,
    /// The id pointing to the current function signature.
    signature: ItemId,
}

impl Function {
    pub fn new(name: String, mangled_name: Option<String>, sig: ItemId) -> Self {
        Function {
            name: name,
            mangled_name: mangled_name,
            signature: sig,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn mangled_name(&self) -> Option<&str> {
        self.mangled_name.as_ref().map(|n| &**n)
    }

    pub fn signature(&self) -> ItemId {
        self.signature
    }
}

/// A function signature.
#[derive(Debug)]
pub struct FunctionSig {
    /// The return type of the function.
    return_type: ItemId,
    /// The type of the arguments, optionally with the name of the argument when
    /// declared.
    argument_types: Vec<(Option<String>, ItemId)>,
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

fn cursor_mangling(cursor: &clang::Cursor, ctx: &BindgenContext) -> Option<String> {
    let mut mangling = cursor.mangling();

    // Try to undo backend linkage munging (prepended _, generally)
    if cfg!(target_os = "macos") {
       // || (cfg!(target_os = "windows") && !ctx.options.msvc_mangling)
        mangling.remove(0);
    }

    if mangling.is_empty() { None } else { Some(mangling) }
}


impl FunctionSig {
    pub fn new(return_type: ItemId,
               arguments: Vec<(Option<String>, ItemId)>,
               is_variadic: bool,
               abi: abi::Abi) -> Self {
        FunctionSig {
            return_type: return_type,
            argument_types: arguments,
            is_variadic: is_variadic,
            is_safe: false,
            abi: abi,
        }
    }

    pub fn from_ty(ty: &clang::Type,
                   cursor: &clang::Cursor,
                   ctx: &mut BindgenContext) -> Result<Self, ParseError> {
        use clangll::*;
        debug!("FunctionSig::from_ty {:?} {:?}", ty, cursor);

        let args: Vec<_> = match cursor.kind() {
            CXCursor_FunctionDecl |
            CXCursor_CXXMethod => {
                // For CXCursor_FunctionDecl, cursor.args() is the reliable way
                // to get parameter names and types.
                cursor.args().iter().map(|arg| {
                    let arg_ty = arg.cur_type();
                    let name = arg.spelling();
                    let name = if name.is_empty() { None } else { Some(name) };
                    let ty = Item::from_ty(&arg_ty, Some(*arg), None, ctx)
                                    .expect("Unable to resolve argument type");
                    (name, ty)
                }).collect()
            }
            _ => {
                // For non-CXCursor_FunctionDecl, visiting the cursor's children
                // is the only reliable way to get parameter names.
                let mut args = vec![];
                cursor.visit(|c, _| {
                    if c.kind() == CXCursor_ParmDecl {
                        let ty = Item::from_ty(&c.cur_type(), Some(*c), None, ctx)
                                    .expect("Unable to resolve arg type");
                        let name = c.spelling();
                        let name = if name.is_empty() { None } else { Some(name) };
                        args.push((name, ty));
                    }
                    CXChildVisit_Continue
                });
                args
            }
        };

        let ret = try!(Item::from_ty(&ty.ret_type(), None, None, ctx));
        let abi = get_abi(ty.call_conv());

        Ok(Self::new(ret, args, ty.is_variadic(), abi))
    }

    pub fn return_type(&self) -> ItemId {
        self.return_type
    }

    pub fn argument_types(&self) -> &[(Option<String>, ItemId)] {
        &self.argument_types
    }

    pub fn abi(&self) -> abi::Abi {
        self.abi
    }

    pub fn is_variadic(&self) -> bool {
        self.is_variadic
    }
}

impl ClangSubItemParser for Function {
    fn parse(cursor: clang::Cursor,
             context: &mut BindgenContext) -> Result<ParseResult<Self>, ParseError> {
        use clangll::*;
        match cursor.kind() {
            CXCursor_FunctionDecl |
            CXCursor_CXXMethod => {},
            _ => return Err(ParseError::Continue),
        };

        // Grab the signature using Item::from_ty.
        let sig = Item::from_ty(&cursor.cur_type(), Some(cursor), None, context)
                        .expect("Expected a function declaration");

        let name = cursor.spelling();
        assert!(!name.is_empty(), "Empty function name?");

        let mut mangled_name = cursor_mangling(&cursor, context);
        if mangled_name.as_ref() == Some(&name) {
            mangled_name = None;
        }

        let function = Self::new(name, mangled_name, sig);
        Ok(ParseResult::New(function, Some(cursor)))
    }
}
