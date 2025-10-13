//! Interpreter-side helpers to build rich diagnostics from runtime paths.

use goblin_diagnostics::{Diagnostic, Severity, Span};

/// Base URL for error docs (anchors appended as #CODE)
pub const ERROR_DOCS_BASE: &str = "https://goblinlang.org/docs/errors";

/// What the interpreter uses to fetch a source line for pretty caret rendering.
pub trait SourceLookup {
    /// Return the exact line text for (file, 1-based line number) if available.
    fn source_line(&self, file: &str, line_no_1based: u32) -> Option<String>;
}

/// Build a runtime error diagnostic (drop-in replacement for old rt(...)).
pub fn rt<L: SourceLookup>(
    lookup: &L,
    code: &'static str,
    category: &'static str,
    message: impl Into<String>,
    primary_span: Span,
) -> Diagnostic {
    let mut d = Diagnostic::runtime(code, category, message, primary_span.clone());
    if let Some(line_text) = lookup.source_line(&primary_span.file, primary_span.line_start) {
        d = d.with_snippet(line_text, primary_span.col_start, primary_span.col_end);
    }
    d
}

pub mod rtcode {
    // ACTION
    pub const UNKNOWN_ACTION: &str = "A0401";

    // FILESYSTEM
    pub const FILESYSTEM_IO: &str = "FS0001";

    // JSON
    pub const JSON_PARSE_FAILED: &str = "J0001";
    pub const JSON_STRINGIFY_FAILED: &str = "J0002";
    pub const JSON_IO: &str = "J0003";
    pub const JSON_WRITE_IO: &str = "J0004";

    // META / CALLABILITY
    pub const MUTATION_OPERATOR_REQUIRED: &str = "M0001";
    pub const NOT_CALLABLE: &str = "M0002";

    // PARSER
    pub const LVALUE_EXPECTED: &str = "P0802";
    pub const FIELD_NAME_REQUIRED: &str = "P0804";
    pub const EXPECTED_ARRAY: &str = "P0314";
    pub const INVALID_NUMBER_LITERAL: &str = "P0330";
    pub const INVALID_DICE_NOTATION: &str = "P0340";
    pub const PICK_MISSING_SOURCE: &str = "P1408";
    pub const READONLY_FIELD: &str = "P9001";

    // RUNTIME
    pub const INTERNAL: &str = "R0000";
    pub const INTERNAL_ASSIGN_SLOT: &str = "R0009";

    pub const UNKNOWN_IDENT: &str = "R0101";
    pub const DUPLICATE_LOCAL: &str = "R0111";
    pub const IMMUTABLE_ASSIGN: &str = "R0113";
    pub const OUTER_SCOPE_SHADOW: &str = "R0114";
    pub const NAMESPACE_NOT_FOUND: &str = "R0115";
    pub const UNKNOWN_CLASS: &str = "R0116";
    pub const UNKNOWN_ENUM: &str = "R0117";
    pub const UNKNOWN_ENUM_VARIANT: &str = "R0118";

    pub const NUMERIC_EXPECTED: &str = "R0200";
    pub const TYPE_MISMATCH_RUNTIME: &str = "R0201";
    pub const BIG_EXPONENT_RANGE: &str = "R0203";
    pub const DIVISION_BY_ZERO: &str = "R0206";
    pub const MATH_DOMAIN: &str = "R0207";
    pub const INVALID_CAST_BIG: &str = "R0212";
    pub const PERCENT_STRING_BIG: &str = "R0223";
    pub const INVALID_NUMERIC_STRING_BIG: &str = "R0224";
    pub const FLOAT_TO_DECIMAL_FAILED: &str = "R0298";

    pub const WRONG_ARITY: &str = "R0301";
    pub const MISSING_ARGUMENT: &str = "R0302";
    pub const PRECISION_LOSS_FLOAT: &str = "R0313";
    pub const INVALID_CAST_FLOAT: &str = "R0314";
    pub const INVALID_CAST_INT: &str = "R0316";
    pub const INVALID_BIG_TO_PCT: &str = "R0320";
    pub const INVALID_PCT_STRING: &str = "R0321";
    pub const INVALID_PCT_CAST: &str = "R0322";
    pub const NON_FINITE_FLOAT: &str = "R0325";
    pub const BIG_OVERFLOW: &str = "R0326";

    pub const INVALID_INDEX: &str = "R0401";
    pub const ARRAY_EXPECTED: &str = "R0402";
    pub const NO_SUCH_FIELD: &str = "R0403";
    pub const EMPTY_ARRAY: &str = "R0404";
    pub const LOOP_CONTROL_OUTSIDE: &str = "R0405";
    pub const MALFORMED_BOUND_ACTION: &str = "R0410";
    pub const RELATION_VALUE_REQUIRED: &str = "R0411";

    pub const UNCLOSED_INTERP_BRACE: &str = "R0500";
    pub const IMPORT_IO: &str = "R0501";
    pub const IMPORT_FAILED: &str = "R0502";
    pub const OP_NOT_MEANINGFUL: &str = "R0503";
    pub const OP_NOT_IMPLEMENTED: &str = "R0504";
    pub const OP_NOT_SUPPORTED: &str = "R0505";

    pub const EMPTY_COLLECTION: &str = "R0701";
    pub const INVALID_RANGE_NO_VALUES: &str = "R0702";
    pub const INSUFFICIENT_DISTINCT: &str = "R0703";
    pub const SAMPLE_TOO_LARGE: &str = "R0704";
    pub const SINGLE_DIE_REQUIRED: &str = "R0705";
    pub const INVALID_OPTION_COMBINATION: &str = "R0706";
    pub const REROLL_EQ_OUT_OF_RANGE: &str = "R0707";
    pub const WEIGHTS_LEN_MISMATCH: &str = "R0708";

    pub const NO_RESULT: &str = "R0902";
    pub const RETURN_ARITY_MISMATCH: &str = "R0903";

    // TYPE
    pub const POSITIVE_INT_EXPECTED: &str = "T0202";
    pub const BOOLEAN_EXPECTED: &str = "T0203";
    pub const INTEGER_EXPECTED: &str = "T0204";
    pub const TYPE_MISMATCH: &str = "T0205";
    pub const NON_NULLABLE_FIELD_REQUIRED: &str = "T0206";
    pub const NON_NULLABLE_FIELD_NIL: &str = "T0207";
    pub const OBJECT_OR_ARRAY_EXPECTED: &str = "T0208";
}
