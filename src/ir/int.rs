#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntKind {
    Bool,
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    // Remember if you add a new variant to add it also to `each`.
}

impl IntKind {
    pub fn is_signed(&self) -> bool {
        use self::IntKind::*;
        match *self {
            Bool | UChar | UShort |
            UInt | ULong | ULongLong => false,

            Char | Short | Int |
            Long | LongLong => false,
        }
    }

    pub fn each<F>(mut cb: F)
        where F: FnMut(Self) -> (),
    {
        use self::IntKind::*;
        cb(Bool);
        cb(Char);
        cb(UChar);
        cb(Short);
        cb(UShort);
        cb(Int);
        cb(UInt);
        cb(Long);
        cb(ULong);
        cb(LongLong);
        cb(ULongLong);
    }
}
