use super::context::TypeResolver;
use super::layout::Layout;
use super::item::ItemId;
use std::cell::Cell;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CompKind {
    Struct,
    Union,
}

/// A struct representing a C++ field.
#[derive(Clone, Debug)]
pub struct Field {
    /// The name of the field.
    name: String,
    /// The inner type.
    ty: ItemId,
    /// The doc comment on the field if any.
    comment: Option<String>,
    /// The set of bitfields this field contains.
    bitfields: Option<Vec<(String, u32)>>,
    /// If the C++ field is marked as `mutable`
    mutable: bool,
}

impl Field {
    pub fn new(name: String,
               ty: ItemId,
               comment: Option<String>,
               bitfields: Option<Vec<(String, u32)>>,
               mutable: bool) -> Field {
        Field {
            name: name,
            ty: ty,
            comment: comment,
            bitfields: bitfields,
            mutable: mutable,
        }
    }
}


#[derive(Debug)]
pub struct CompInfo {
    /// Whether this is a struct or a union.
    kind: CompKind,
    /// The members of this struct or union.
    members: Vec<Field>,
    /// The template parameters of this class. These are non-concrete, and
    /// should always be a Type(TypeKind::Named(name)), but still they need to
    /// be registered with an unique type id in the context.
    template_args: Vec<ItemId>,
    /// The method declarations inside this class, if in C++ mode.
    methods: Vec<ItemId>,
    /// Virtual method declarations.
    virtual_methods: Vec<ItemId>,
    /// Vector of classes this one inherits from.
    base_members: Vec<ItemId>,
    /// The parent reference template if any.
    ref_template: Option<ItemId>,
    /// The inner types that were declared inside this class, in something like:
    ///
    /// class Foo {
    ///     typedef int FooTy;
    ///     struct Bar {
    ///         int baz;
    ///     };
    /// }
    ///
    /// static Foo::Bar const = {3};
    inner_types: Vec<ItemId>,
    /// Set of static constants declared inside this class.
    vars: Vec<ItemId>,
    /// Whether this type should generate an vtable (TODO: Should be able to
    /// look at the virtual methods and ditch this field).
    has_vtable: bool,
    /// Whether this type has destructor.
    has_destructor: bool,
    /// Whether this type has a base type with more than one member.
    ///
    /// TODO: We should be able to compute this.
    has_nonempty_base: bool,
    /// If this type has a template parameter which is not a type (e.g.: a size_t)
    has_non_type_template_params: bool,
    /// Used to detect if we've run in a can_derive_debug cycle while cycling
    /// around the template arguments.
    detect_derive_debug_cycle: Cell<bool>,
    /// Used to detect if we've run in a has_destructor cycle while cycling
    /// around the template arguments.
    detect_has_destructor_cycle: Cell<bool>,
}

impl CompInfo {
    pub fn new(kind: CompKind) -> Self {
        CompInfo {
            kind: kind,
            members: vec![],
            template_args: vec![],
            methods: vec![],
            virtual_methods: vec![],
            base_members: vec![],
            ref_template: None,
            inner_types: vec![],
            vars: vec![],
            has_vtable: false,
            has_destructor: false,
            has_nonempty_base: false,
            has_non_type_template_params: false,
            detect_derive_debug_cycle: Cell::new(false),
            detect_has_destructor_cycle: Cell::new(false),
        }
    }

    pub fn can_derive_debug(&self, type_resolver: &TypeResolver) -> bool {
        // We can reach here recursively via template parameters of a member,
        // for example.
        if self.detect_derive_debug_cycle.get() {
            warn!("Derive debug cycle detected!");
            return true;
        }

        match self.kind {
            CompKind::Union => {
                // XXX: Move this logic to the Type type.
                // let size_divisor = if self.layout.align == 0 { 1 } else { self.layout.align };
                // if self.layout.size / size_divisor > 32 {
                //     return false;
                // }
                true
            }
            CompKind::Struct => {
                self.detect_derive_debug_cycle.set(true);

                let can_derive_debug =
                    self.template_args.iter().all(|ty| {
                        type_resolver.resolve_type(*ty)
                                     .can_derive_debug(type_resolver)
                    }) &&
                    self.members.iter().all(|field| {
                        type_resolver.resolve_type(field.ty)
                                     .can_derive_debug(type_resolver)
                    });

                self.detect_derive_debug_cycle.set(false);

                can_derive_debug
            }
        }
    }

    pub fn has_destructor(&self, type_resolver: &TypeResolver) -> bool {
        if self.detect_has_destructor_cycle.get() {
            warn!("Cycle detected looking for destructors");
            // Assume no destructor, since we don't have an explicit one.
            return false;
        }

        self.detect_has_destructor_cycle.set(true);

        let has_destructor = self.has_destructor || match self.kind {
            CompKind::Union => false,
            CompKind::Struct => {
                // NB: We can't rely on a type with type parameters
                // not having destructor.
                //
                // This is unfortunate, but...
                self.ref_template.as_ref().map_or(false, |t| {
                    type_resolver.resolve_type(*t).has_destructor(type_resolver)
                }) ||
                self.template_args.iter().any(|t| {
                    type_resolver.resolve_type(*t).has_destructor(type_resolver)
                }) ||
                self.members.iter().any(|field| {
                    type_resolver.resolve_type(field.ty)
                                 .has_destructor(type_resolver)
                })
            }
        };

        self.detect_has_destructor_cycle.set(false);

        has_destructor
    }

    pub fn can_derive_copy(&self, type_resolver: &TypeResolver) -> bool {
        // NOTE: Take into account that while unions in C and C++ are copied by
        // default, the may have an explicit destructor in C++, so we can't
        // defer this check just for the union case.
        if self.has_destructor(type_resolver) {
            return false;
        }

        match self.kind {
            CompKind::Union => true,
            CompKind::Struct => {
                // With template args, use a safe subset of the types,
                // since copyability depends on the types itself.
                self.ref_template.as_ref().map_or(true, |t| {
                    type_resolver.resolve_type(*t).has_destructor(type_resolver)
                }) ||
                self.members.iter().all(|field| {
                    type_resolver.resolve_type(field.ty)
                                 .has_destructor(type_resolver)
                })
            }
        }
    }

    // Computes the layout of this type.
    //
    // This is called as a fallback under some circumstances where LLVM doesn't
    // give us the correct layout.
    // If we're a union without known layout, we try to compute it from our
    // members. This is not ideal, but clang fails to report the size for
    // these kind of unions, see test/headers/template_union.hpp
    pub fn layout(&self, type_resolver: &TypeResolver) -> Option<Layout> {
        use std::cmp;

        // We can't do better than clang here, sorry.
        if self.kind == CompKind::Struct {
            return None;
        }

        let mut max_size = 0;
        let mut max_align = 0;
        for field in &self.members {
            let field_layout = type_resolver.resolve_type(field.ty)
                                            .layout(type_resolver);

            if let Some(layout) = field_layout {
                max_size = cmp::max(max_size, layout.size);
                max_align = cmp::max(max_align, layout.align);
            }
        }

        Some(Layout::new(max_size, max_align))
    }

}
