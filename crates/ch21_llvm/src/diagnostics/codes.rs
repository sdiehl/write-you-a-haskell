//! The stable error registry, also consumed by the book's error index generator.
macro_rules! codes {
    ($($variant:ident => ($code:literal, $description:literal)),* $(,)?) => {
        #[derive(Clone,Copy,Debug,PartialEq,Eq)]
        /// Stable diagnostic identities survive wording and rendering changes.
        pub enum ErrorCode { $($variant),* }
        impl ErrorCode {
            pub fn as_str(self) -> &'static str { match self { $(Self::$variant => $code),* } }
            pub fn description(self) -> &'static str { match self { $(Self::$variant => $description),* } }
        }
        pub const REGISTRY: &[(ErrorCode, &str)] = &[$((ErrorCode::$variant, $description)),*];
    };
}

codes! {
    Lex => ("E0001", "invalid token"), Parse => ("E0002", "invalid syntax"),
    Layout => ("E0003", "invalid layout"), ProjectionDot => ("E0004", "invalid projection dot"),
    Unbound => ("E0101", "unbound variable"), Duplicate => ("E0102", "duplicate definition"),
    UnboundConstructor => ("E0103", "unbound constructor or type"), DuplicateField => ("E0104", "duplicate field"),
    ReservedOperator => ("E0110", "reserved operator redefined"), PrivatePrimitive => ("E0111", "primitive outside Prelude"),
    RecursiveValue => ("E0120", "recursive non-function binding"), OrphanFixity => ("E0130", "fixity without definition"),
    Mismatch => ("E0201", "type mismatch"), Occurs => ("E0202", "infinite type"), Signature => ("E0203", "signature too general"),
    NoInstance => ("E0210", "no instance"), Instance => ("E0211", "invalid instance methods or duplicate instance"), Ambiguous => ("E0230", "ambiguous type"),
    RecordConstructors => ("E0240", "record must have one constructor"), MissingField => ("E0241", "missing field"),
    RepeatedField => ("E0242", "duplicate field binding"), UnknownField => ("E0243", "unknown field"),
    NoField => ("E0244", "type does not declare field"), AmbiguousField => ("E0245", "ambiguous field access"),
    Main => ("E0250", "main must have type IO ()"),
    Kind => ("E0301", "kind mismatch"), PartialSynonym => ("E0302", "partially applied synonym"), SynonymCycle => ("E0303", "cyclic synonym"),
    NonExhaustive => ("W0001", "non-exhaustive match"), Redundant => ("W0002", "redundant clause"),
}
