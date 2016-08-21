use super::int::IntKind;

#[derive(Debug)]
pub struct Enum {
    /// The representation used for this enum.
    repr: IntKind,
    /// The different variants, with explicit values.
    variants: Vec<EnumVariant>,
}

impl Enum {
    pub fn new(repr: IntKind) -> Self {
        Enum {
            repr: repr,
            variants: vec![],
        }
    }

    pub fn repr(&self) -> IntKind {
        self.repr
    }

    pub fn variants(&self) -> &[EnumVariant] {
        &self.variants
    }
}

/// A single enum variant, to be contained only in an enum.
#[derive(Debug)]
pub struct EnumVariant {
    /// The name of the variant.
    name: String,
    /// An optional doc comment.
    comment: Option<String>,
    /// The integer value of the variant.
    val: i64
}

impl EnumVariant {
    pub fn new(name: String, comment: Option<String>, val: i64) -> Self {
        EnumVariant {
            name: name,
            comment: comment,
            val: val,
        }
    }
}
