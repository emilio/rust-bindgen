use ir::context::BindgenContext;
use ir::item::{Item, ItemId};
use ir::ty::{Type, TypeKind};
use ir::int::IntKind;
use ir::var::Var;
use ir::enum_ty::Enum;
use ir::function::{Function, FunctionSig};
use ir::item_kind::ItemKind;
use ir::comp::{CompKind, CompInfo, Method};
use ir::layout::Layout;

use std::collections::hash_map::{HashMap, Entry};

use syntax::abi::Abi;
use syntax::ast;
use syntax::codemap::{Span, respan, ExpnInfo, NameAndSpan, MacroBang};
use syntax::ext::base;
use syntax::ext::build::AstBuilder;
use syntax::ext::expand::ExpansionConfig;
use syntax::ext::quote::rt::ToTokens;
use syntax::feature_gate::Features;
use syntax::parse;
use syntax::parse::token::{InternedString, intern};
use syntax::attr::mk_attr_id;
use syntax::ptr::P;
use syntax::print::pprust::tts_to_string;
use aster;

macro_rules! repr {
    ($which:expr) => {{
        aster::AstBuilder::new().attr().list("repr").words(&[$which]).build()
    }}
}

macro_rules! doc {
    ($comment:expr) => {{
        aster::AstBuilder::new().attr().doc($comment)
    }}
}

macro_rules! link_name {
    ($name:expr) => {{
        aster::AstBuilder::new().attr().name_value("link_name").str($name)
    }}
}

struct ForeignModBuilder {
    inner: ast::ForeignMod,
}

impl ForeignModBuilder {
    fn new(abi: Abi) -> Self {
        ForeignModBuilder {
            inner: ast::ForeignMod {
                abi: abi,
                items: vec![],
            }
        }
    }

    fn with_foreign_item(mut self, item: ast::ForeignItem) -> Self {
        self.inner.items.push(item);
        self
    }

    fn with_foreign_items<I>(mut self, items: I) -> Self
        where I: IntoIterator<Item=ast::ForeignItem>
    {
        self.inner.items.extend(items.into_iter());
        self
    }

    fn build(self, ctx: &BindgenContext) -> P<ast::Item> {
        use syntax::codemap::DUMMY_SP;
        P(ast::Item {
            ident: ctx.rust_ident(""),
            id: ast::DUMMY_NODE_ID,
            node: ast::ItemKind::ForeignMod(self.inner),
            vis: ast::Visibility::Public,
            attrs: vec![],
            span: DUMMY_SP,
        })
    }
}



struct ArrayTyBuilder {
    len: usize,
}

// TODO: Aster seem to lack a array builder?
impl ArrayTyBuilder {
    fn new() -> Self {
        ArrayTyBuilder {
            len: 0,
        }
    }

    fn with_len(mut self, len: usize) -> Self {
        self.len = len;
        self
    }

    fn build(self, ty: P<ast::Ty>) -> P<ast::Ty> {
        use syntax::codemap::DUMMY_SP;
        let size =
            ast::LitKind::Int(self.len as u64,
                              ast::LitIntType::Unsigned(ast::UintTy::Us));
        let size = ast::ExprKind::Lit(P(respan(DUMMY_SP, size)));
        let array_kind = ast::TyKind::FixedLengthVec(ty,
            P(ast::Expr {
                id: ast::DUMMY_NODE_ID,
                node: size,
                span: DUMMY_SP,
                attrs: ast::ThinVec::new(),
            })
        );

        P(ast::Ty {
            id: ast::DUMMY_NODE_ID,
            node: array_kind,
            span: DUMMY_SP,
        })
    }
}

struct BlobTyBuilder {
    layout: Layout,
}

impl BlobTyBuilder {
    fn new(layout: Layout) -> Self {
        BlobTyBuilder {
            layout: layout,
        }
    }

    fn build(self) -> P<ast::Ty> {
        use std::cmp;

        let ty_name = match self.layout.align {
            8 => "u64",
            4 => "u32",
            2 => "u16",
            1 | _ => "u8",
        };
        let data_len = if ty_name == "u8" {
            self.layout.size
        } else {
            self.layout.size / cmp::max(self.layout.align, 1)
        };

        let inner_ty = aster::AstBuilder::new().ty().path().id(ty_name).build();
        ArrayTyBuilder::new().with_len(data_len).build(inner_ty)
    }
}

/// A trait to convert a rust type into a pointer, optionally const, to the same
/// type.
///
/// This is done due to aster's lack of pointer builder, I guess I should PR
/// there.
trait ToPtr {
    fn to_ptr(self, is_const: bool, span: Span) -> P<ast::Ty>;
}

impl ToPtr for P<ast::Ty> {
    fn to_ptr(self, is_const: bool, span: Span) -> Self {
        let ty = ast::TyKind::Ptr(ast::MutTy {
            ty: self,
            mutbl: if is_const {
                ast::Mutability::Immutable
            } else {
                ast::Mutability::Mutable
            }
        });
        P(ast::Ty {
            id: ast::DUMMY_NODE_ID,
            node: ty,
            span: span,
        })
    }
}

/// A trait to get the canonical name from an item.
///
/// This is the trait that will eventually isolate all the logic related to name
/// mangling and that kind of stuff.
///
/// This assumes no nested paths, at some point I'll have to make it a more
/// complex thing.
///
/// This name is required to be safe for Rust, that is, is not expected to
/// return any rust keyword from here.
trait ItemCanonicalName {
    fn canonical_name(&self, ctx: &BindgenContext) -> String;
}

impl ItemCanonicalName for Item {
    fn canonical_name(&self, ctx: &BindgenContext) -> String {
        let base_name = match *self.kind() {
            ItemKind::Type(ref ty) => {
                // If we're a template specialization, our name is our parent's
                if let TypeKind::Comp(ref ci) = *ty.kind() {
                    if ci.is_template_specialization() {
                        return ci.specialized_template().unwrap().canonical_name(ctx)
                    }
                }
                if let TypeKind::TemplateRef(inner, _) = *ty.kind() {
                    return inner.canonical_name(ctx)
                }
                ty.name().map(ToOwned::to_owned)
                         .unwrap_or_else(|| format!("_bindgen_ty{}", self.id()))
            }
            ItemKind::Function(ref fun) => {
                fun.name().to_owned()
            }
            ItemKind::Var(ref var) => {
                var.name().to_owned()
            }
            ItemKind::Module(ref module) => unimplemented!(), // TODO
        };

        if self.is_toplevel(ctx) {
            return ctx.rust_mangle(&base_name).into_owned();
        }

        format!("{}_{}", self.parent_id().canonical_name(ctx), base_name)
    }
}

impl ItemCanonicalName for ItemId {
    fn canonical_name(&self, ctx: &BindgenContext) -> String {
        ctx.resolve_item(*self).canonical_name(ctx)
    }
}

trait CodeGenerator {
    /// Extra information from the caller.
    type Extra;

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               extra: &Self::Extra);
}

impl CodeGenerator for Item {
    type Extra = ();

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               _extra: &()) {
        match *self.kind() {
            ItemKind::Module(..) => { /* TODO */ },
            ItemKind::Function(ref fun) => {
                fun.codegen(ctx, result, self);
            },
            ItemKind::Var(ref var) => {
                var.codegen(ctx, result, self);
            },
            ItemKind::Type(ref ty) => {
                ty.codegen(ctx, result, self);
            }
        }
    }
}

impl CodeGenerator for Var {
    type Extra = Item;
    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               item: &Item) {
        let name = item.canonical_name(ctx);
        let ty = self.ty().to_rust_ty(ctx);

        if let Some(val) = self.val() {
            let val = self.val().unwrap();
            let const_item = aster::AstBuilder::new().item().pub_().const_(name)
                                                .expr().int(val).build(ty);
            result.push(const_item)
        } else {
            let mut attrs = vec![];
            if let Some(mangled) = self.mangled_name() {
                attrs.push(link_name!(mangled));
            } else if name != self.name() {
                attrs.push(link_name!(self.name()));
            }

            let item = ast::ForeignItem {
                ident: ctx.rust_ident_raw(&*name),
                attrs: attrs,
                node: ast::ForeignItemKind::Static(ty, !self.is_const()),
                id: ast::DUMMY_NODE_ID,
                span: ctx.span(),
                vis: ast::Visibility::Public,
            };

            let item = ForeignModBuilder::new(Abi::C)
                            .with_foreign_item(item)
                            .build(ctx);
            result.push(item);
        }
    }
}

impl CodeGenerator for Type {
    type Extra = Item;

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               item: &Item) {
        match *self.kind() {
            TypeKind::Void |
            TypeKind::NullPtr |
            TypeKind::Int(..) |
            TypeKind::Float(..) |
            TypeKind::Array(..) |
            TypeKind::Pointer(..) |
            TypeKind::Reference(..) |
            TypeKind::TemplateRef(..) |
            TypeKind::Function(..) |
            TypeKind::Named(..) => {
                // These items don't need code generation, they only need to be
                // converted to rust types in fields, arguments, and such.
                return;
            }
            TypeKind::Comp(ref ci) => ci.codegen(ctx, result, item),
            TypeKind::Alias(_, inner_item) => {
                let inner_item = ctx.resolve_item(inner_item);
                let name = item.canonical_name(ctx);

                // Try to catch the common pattern:
                //
                // typedef struct foo { ... } foo;
                //
                // here.
                //
                if inner_item.canonical_name(ctx) == name {
                    return;
                }

                let inner_rust_type = inner_item.to_rust_ty(ctx);
                let rust_name = ctx.rust_ident(&name);
                let mut typedef = aster::AstBuilder::new().item().pub_();

                if let Some(comment) = item.comment() {
                    typedef = typedef.attr().doc(comment);
                }

                let typedef = typedef.type_(&rust_name)
                                     .build_ty(inner_rust_type);
                result.push(typedef)
            }
            TypeKind::Enum(ref ei) => ei.codegen(ctx, result, item),
        }
    }
}

struct Vtable<'a> {
    item_id: ItemId,
    methods: &'a [Method],
    base_classes: &'a [ItemId],
}

impl<'a> Vtable<'a> {
    fn new(item_id: ItemId, methods: &'a [Method], base_classes: &'a [ItemId]) -> Self {
        Vtable {
            item_id: item_id,
            methods: methods,
            base_classes: base_classes,
        }
    }
}

impl<'a> CodeGenerator for Vtable<'a> {
    type Extra = Item;

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               item: &Item) {
        assert_eq!(item.id(), self.item_id);
        // For now, generate an empty struct, later we should generate function
        // pointers and whatnot.
        let vtable = aster::AstBuilder::new().item().pub_()
                            .with_attr(repr!("C"))
                            .struct_(self.canonical_name(ctx))
                            .build();
        result.push(vtable);
    }
}

impl<'a> ItemCanonicalName for Vtable<'a> {
    fn canonical_name(&self, ctx: &BindgenContext) -> String {
        format!("bindgen_vtable_{}", self.item_id)
    }
}

impl CodeGenerator for CompInfo {
    type Extra = Item;

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               item: &Item) {
        use aster::struct_field::StructFieldBuilder;
        // Don't output classes with template parameters that aren't types, and
        // also don't output template specializations, neither total or partial.
        if self.has_non_type_template_params() || self.is_template_specialization() {
            return;
        }

        let mut attributes = vec![];
        if let Some(comment) = item.comment() {
            attributes.push(doc!(comment));
        }
        attributes.push(repr!("C"));

        let mut template_args_used = vec![false; self.template_args().len()];
        let is_union = self.kind() == CompKind::Union;

        let canonical_name = item.canonical_name(ctx);
        let mut builder = aster::AstBuilder::new().item().pub_()
                                .with_attrs(attributes)
                                .struct_(&canonical_name);

        // Generate the vtable from the method list if appropriate.
        // TODO: I don't know how this could play with virtual methods that are
        // not in the list of methods found by us, we'll see. Also, could the
        // order of the vtable pointers vary?
        let mut fields = vec![];
        if self.has_vtable() {
            let vtable = Vtable::new(item.id(),
                                     self.methods(),
                                     self.base_members());
            vtable.codegen(ctx, result, item);

            let vtable_field = StructFieldBuilder::named("vtable_").pub_()
                                    .ty().id(vtable.canonical_name(ctx));

            fields.push(vtable_field);
        }

        for (i, base) in self.base_members().iter().enumerate() {
            let base_ty = ctx.resolve_type(*base);
            for (i, ty) in self.template_args().iter().enumerate() {
                if base_ty.signature_contains_named_type(ctx, ctx.resolve_type(*ty)) {
                    template_args_used[i] = true;
                }
            }

            let inner = base.to_rust_ty(ctx);
            let field = StructFieldBuilder::named(format!("base_{}", i))
                                    .pub_().build_ty(inner);
            fields.push(field);
        }

        for field in self.fields() {
            if let Some(_width) = field.bitfield() {
                // TODO: Compute bitfields.
                continue;
            }

            let field_ty = ctx.resolve_type(field.ty());
            for (i, ty) in self.template_args().iter().enumerate() {
                if field_ty.signature_contains_named_type(ctx, ctx.resolve_type(*ty)) {
                    template_args_used[i] = true;
                }
            }

            let ty = field.ty().to_rust_ty(ctx);
            let mut attrs = vec![];
            if let Some(comment) = field.comment() {
                attrs.push(doc!(comment));
            }

            let field = StructFieldBuilder::named(field.name()).pub_()
                                    .with_attrs(attrs)
                                    .build_ty(ty);
            fields.push(field);
        }

        // Append any extra template arguments that nobody has used so far.
        for (i, ty) in self.template_args().iter().enumerate() {
            if !template_args_used[i] {
                let name = ctx.resolve_type(*ty).name().unwrap();
                let ident = ctx.rust_ident(name);
                let field =
                    StructFieldBuilder::named(format!("_phantom_{}", i)).pub_()
                            .build_ty(quote_ty!(ctx.ext_cx(), ::std::marker::PhantomData<$ident>));
                fields.push(field)
            }
        }

        let mut generics = aster::AstBuilder::new().generics();
        for template_arg in self.template_args() {
            // Take into account that here only arrive named types, not
            // template specialisations that would need to be
            // instantiated.
            //
            // TODO: Add template args from the parent, here and in
            // `to_rust_ty`!!
            let template_arg = ctx.resolve_type(*template_arg);
            generics = generics.ty_param_id(template_arg.name().unwrap());
        }

        let rust_struct = builder.with_generics(generics.build())
                                 .with_fields(fields).build();
        result.push(rust_struct);

        // Generate the inner types and all that stuff.
        //
        // TODO: In the future we might want to be smart, and use nested
        // modules, and watnot.
        for ty in self.inner_types() {
            ctx.resolve_item(*ty).codegen(ctx, result, &());
        }

        for method in self.methods() {
            if !method.is_virtual() {
                ctx.resolve_item(method.signature()).codegen(ctx, result, &());
            }
        }

        // TODO: test fns, etc.
    }
}

impl CodeGenerator for Enum {
    type Extra = Item;

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               item: &Item) {
        use ir::enum_ty::EnumVariantValue;

        let name = item.canonical_name(ctx);
        let layout = item.expect_type().layout(ctx);

        let repr = self.repr().map(|repr| ctx.resolve_type(repr));
        let repr = match repr {
            Some(repr) => match *repr.kind() {
                TypeKind::Int(int_kind) => int_kind,
                _ => panic!("Unexpected type as enum repr"),
            },
            None => {
                warn!("Guessing type of enum! Forward declarations of enums shouldn't be legal!");
                IntKind::Int
            }
        };

        let signed = repr.is_signed();
        let size = layout.map(|l| l.size).unwrap_or(0);
        let repr_name = match (signed, size) {
            (true, 1) => "i8",
            (false, 1) => "u8",
            (true, 2) => "i16",
            (false, 2) => "u16",
            (true, 4) => "i32",
            (false, 4) => "u32",
            (true, 8) => "i64",
            (false, 8) => "u64",
            _ => {
                warn!("invalid enum decl: signed: {}, size: {}", signed, size);
                "i32"
            }
        };

        let mut builder = aster::AstBuilder::new().item().pub_();

        // FIXME: Rust forbids repr with empty enums. Remove this condition when
        // this is allowed.
        if !self.variants().is_empty() {
            builder = builder.with_attr(repr!(repr_name));
        }

        if let Some(comment) = item.comment() {
            builder = builder.with_attr(doc!(comment));
        }

        let mut builder = builder.enum_(&name);

        // A map where we keep a value -> variant relation.
        let mut seen_values = HashMap::<_, String>::new();
        for variant in self.variants().iter() {
            match seen_values.entry(variant.val()) {
                Entry::Occupied(ref entry) => {
                    let enum_variant_name = entry.get();
                    // TODO: the constant name should be something like
                    // ${enum_name}_${variant_name} or something, I guess.
                    let constant = aster::AstBuilder::new().item().pub_()
                                                           .const_(variant.name())
                                                           .expr().path()
                                                           .ids(&[&*name, &*enum_variant_name])
                                                           .build().build(item.to_rust_ty(ctx));
                    result.push(constant);
                }
                Entry::Vacant(entry) => {
                    let expr = aster::AstBuilder::new().expr();
                    let expr = match variant.val() {
                        EnumVariantValue::Signed(val) => expr.int(val),
                        EnumVariantValue::Unsigned(val) => expr.uint(val),
                    };
                    builder = builder.with_variant_(ast::Variant_ {
                        name: ctx.rust_ident(variant.name()),
                        attrs: vec![],
                        data: ast::VariantData::Unit(ast::DUMMY_NODE_ID),
                        disr_expr: Some(expr),
                    });
                    entry.insert(variant.name().into());
                }
            }
        }


        result.push(builder.build());
    }
}

trait ToRustTy {
    type Extra;

    fn to_rust_ty(&self, ctx: &BindgenContext, extra: &Self::Extra) -> P<ast::Ty>;
}

trait ItemToRustTy {
    fn to_rust_ty(&self, ctx: &BindgenContext) -> P<ast::Ty>;
}

// Convenience implementation.
impl ItemToRustTy for ItemId {
    fn to_rust_ty(&self, ctx: &BindgenContext) -> P<ast::Ty> {
        debug!("to_rust_ty: {:?}", self);
        ctx.resolve_item(*self).to_rust_ty(ctx)
    }
}

impl ItemToRustTy for Item {
    fn to_rust_ty(&self, ctx: &BindgenContext) -> P<ast::Ty> {
        self.kind().expect_type().to_rust_ty(ctx, self)
    }
}

fn raw_type(ctx: &BindgenContext, name: &str) -> P<ast::Ty> {
    let ident = ctx.rust_ident_raw(name);
    quote_ty!(ctx.ext_cx(), ::std::os::raw::$ident)
}

impl ToRustTy for Type {
    type Extra = Item;

    fn to_rust_ty(&self, ctx: &BindgenContext, item: &Item) -> P<ast::Ty> {
        macro_rules! raw {
            ($ty: ident) => {
                raw_type(ctx, stringify!($ty))
            }
        }
        match *self.kind() {
            TypeKind::Void => raw!(c_void),
            // TODO: we should do something smart with nullptr, or maybe *const
            // c_void is enough?
            TypeKind::NullPtr => quote_ty!(ctx.ext_cx(), *const ::std::os::raw::c_void),
            TypeKind::Int(ik) => {
                match ik {
                    IntKind::Bool => aster::ty::TyBuilder::new().bool(),
                    IntKind::Char => raw!(c_char),
                    IntKind::UChar => raw!(c_uchar),
                    IntKind::Short => raw!(c_short),
                    IntKind::UShort => raw!(c_ushort),
                    IntKind::Int => raw!(c_int),
                    IntKind::UInt => raw!(c_uint),
                    IntKind::Long => raw!(c_long),
                    IntKind::ULong => raw!(c_ulong),
                    IntKind::LongLong => raw!(c_longlong),
                    IntKind::ULongLong => raw!(c_ulonglong),
                }
            }
            TypeKind::Float(fk) => {
                use ir::ty::FloatKind;
                // TODO: we probably should just take the type layout into
                // account?
                match fk {
                    FloatKind::Float => aster::ty::TyBuilder::new().f32(),
                    FloatKind::Double |
                    FloatKind::LongDouble => aster::ty::TyBuilder::new().f64(),
                }
            }
            TypeKind::Function(ref fs) => {
                let ty = fs.to_rust_ty(ctx, item);
                aster::AstBuilder::new().ty().option().build(ty)
            }
            TypeKind::Array(item, len) => {
                let inner = item.to_rust_ty(ctx);
                ArrayTyBuilder::new().with_len(len).build(inner)
            }
            TypeKind::Alias(_, _inner) => {
                // TODO: This will have to change once we support modules
                // properly. Paths will need to change.
                let ident = ctx.rust_ident(&item.canonical_name(ctx));
                quote_ty!(ctx.ext_cx(), $ident)
            }
            TypeKind::Enum(..) => {
                let name = item.canonical_name(ctx);
                let ident = ctx.rust_ident(&name);
                quote_ty!(ctx.ext_cx(), $ident)
            }
            TypeKind::TemplateRef(inner, ref template_args) => {
                // TODO: we might need to do this recursive to take alias into
                // account?
                //
                // PS: Sorry for the duplication here.
                let mut inner_ty = inner.to_rust_ty(ctx).unwrap();

                println!("TemplateRef, {:?}", template_args);

                if let ast::TyKind::Path(_, ref mut path) = inner_ty.node {
                    path.segments.last_mut().unwrap().parameters =
                        ast::PathParameters::AngleBracketed(
                            ast::AngleBracketedParameterData {
                                lifetimes: vec![],
                                types: P::from_vec(template_args.iter().map(|arg| {
                                    arg.to_rust_ty(ctx)
                                }).collect()),
                                bindings: P::from_vec(vec![]),
                            }
                        );
                }

                return P(inner_ty);
            }
            TypeKind::Comp(ref info) => {
                if info.has_non_type_template_params() {
                    let layout = self.layout(ctx)
                                     .expect("Couldn't compute layout for type with non-type template parameters");
                    return BlobTyBuilder::new(layout).build();
                }

                let name = item.canonical_name(ctx);
                let ident = ctx.rust_ident(&name);
                aster::AstBuilder::new().ty().path()
                    .segment(&name)
                    .with_tys(info.template_args().iter().map(|arg| {
                        arg.to_rust_ty(ctx)
                    }))
                    .build().build()
            }
            TypeKind::Pointer(inner) |
            TypeKind::Reference(inner) => {
                inner.to_rust_ty(ctx).to_ptr(self.is_const(), ctx.span())
            }
            TypeKind::Named(ref name) => {
                let ident = ctx.rust_ident(name);
                quote_ty!(ctx.ext_cx(), $ident)
            }
        }
    }
}

impl ToRustTy for FunctionSig {
    type Extra = Item;

    fn to_rust_ty(&self, ctx: &BindgenContext, item: &Item) -> P<ast::Ty> {
        // TODO: we might want to consider ignoring the reference return value.
        let return_item = ctx.resolve_item(self.return_type());
        let ret = if let TypeKind::Void = *return_item.kind().expect_type().kind() {
            ast::FunctionRetTy::Default(ctx.span())
        } else {
            ast::FunctionRetTy::Ty(return_item.to_rust_ty(ctx))
        };

        let mut unnamed_arguments = 0;
        let arguments = self.argument_types().iter().map(|&(ref name, ty)| {
            let arg_item = ctx.resolve_item(ty);
            let arg_ty = arg_item.kind().expect_type();

            // From the C90 standard (http://c0x.coding-guidelines.com/6.7.5.3.html)
            // 1598 - A declaration of a parameter as “array of type” shall be
            // adjusted to “qualified pointer to type”, where the type qualifiers
            // (if any) are those specified within the [ and ] of the array type
            // derivation.
            let arg_ty = if let TypeKind::Array(t, _) = *arg_ty.kind() {
                t.to_rust_ty(ctx).to_ptr(arg_ty.is_const(), ctx.span())
            } else {
                arg_item.to_rust_ty(ctx)
            };

            let arg_name = match *name {
                Some(ref name) => name.clone(),
                None => {
                    unnamed_arguments += 1;
                    format!("arg{}", unnamed_arguments)
                }
            };

            ast::Arg {
                ty: arg_ty,
                pat: aster::AstBuilder::new().pat().id(arg_name),
                id: ast::DUMMY_NODE_ID,
            }
        }).collect::<Vec<_>>();

        let decl = P(ast::FnDecl {
            inputs: arguments,
            output: ret,
            variadic: self.is_variadic(),
        });

        let fnty = ast::TyKind::BareFn(P(ast::BareFnTy {
            unsafety: ast::Unsafety::Unsafe,
            abi: self.abi(),
            lifetimes: vec![],
            decl: decl,
        }));

        P(ast::Ty {
            id: ast::DUMMY_NODE_ID,
            node: fnty,
            span: ctx.span(),
        })
    }
}

impl CodeGenerator for Function {
    type Extra = Item;

    fn codegen(&self,
               ctx: &BindgenContext,
               result: &mut Vec<P<ast::Item>>,
               item: &Item) {
        let signature_item = ctx.resolve_item(self.signature());
        let signature = signature_item.kind().expect_type();
        let signature = match *signature.kind() {
            TypeKind::Function(ref sig) => sig,
            _ => panic!("How?"),
        };

        // NB: we do kind().to_rust_ty(), to avoid the type being wrapped in an
        // Option<>.
        let decl_ty = signature.to_rust_ty(ctx, signature_item);

        let fndecl = match decl_ty.unwrap().node {
            ast::TyKind::BareFn(bare_fn) => {
                bare_fn.unwrap().decl
            }
            _ => panic!("How did this happen exactly?"),
        };

        let name = self.name();
        let canonical_name = item.canonical_name(ctx);

        let mut attributes = vec![];

        if let Some(comment) = item.comment() {
            attributes.push(doc!(comment));
        }

        if let Some(mangled) = self.mangled_name() {
            attributes.push(link_name!(mangled));
        } else if name != canonical_name {
            attributes.push(link_name!(name));
        }

        let foreign_item_kind =
            ast::ForeignItemKind::Fn(fndecl, ast::Generics::default());

        let foreign_item =
            ast::ForeignItem {
                ident: ctx.rust_ident_raw(canonical_name),
                attrs: attributes,
                node: foreign_item_kind,
                id: ast::DUMMY_NODE_ID,
                span: ctx.span(),
                vis: ast::Visibility::Public,
            };

        let item = ForeignModBuilder::new(signature.abi())
            .with_foreign_item(foreign_item)
            .build(ctx);

        result.push(item);
    }
}

pub fn codegen(context: &mut BindgenContext) -> Vec<P<ast::Item>> {
    context.gen(|context| {
        let mut result = vec![];
        for (item_id, item) in context.items() {
            // Non-toplevel item parents are the responsible one for generating
            // them.
            if item.is_toplevel(context) {
                item.codegen(context, &mut result, &());
            }
        }
        result
    })
}
