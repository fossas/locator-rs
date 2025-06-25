use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

/// Records all errors reported by this library.
#[derive(Error, Diagnostic, Debug)]
#[non_exhaustive]
pub enum Error {
    /// Errors encountered while parsing.
    #[error(transparent)]
    Parse(#[from] ParseError),
}

/// Errors encountered when parsing.
#[derive(Error, Diagnostic, Debug)]
#[non_exhaustive]
pub enum ParseError {
    /// The provided value was empty, which is invalid for this type.
    #[error("input was empty, which is invalid for this type")]
    Empty,

    /// The input was expected to exactly match the target value,
    /// but was something else.
    #[error("input literal '{input}' did not match the target value '{expected}'")]
    LiteralExact {
        /// The input originally provided.
        #[source_code]
        input: String,

        /// The expected value.
        expected: String,

        /// The location of the error.
        #[label("field")]
        span: SourceSpan,
    },

    /// The input was expected to be one of the provided options,
    /// but was something else.
    #[error("input literal '{input}' did not match one of: {}", options.join(", "))]
    LiteralOneOf {
        /// The input originally provided.
        #[source_code]
        input: String,

        /// The possible accepted options.
        options: Vec<String>,

        /// The location of the error.
        #[label("field")]
        span: SourceSpan,
    },

    /// The input did not match the required syntax.
    #[error("input '{input}' did not match required syntax: {error}")]
    Syntax {
        /// The input originally provided.
        #[source_code]
        input: String,

        /// The error encountered while parsing.
        #[source]
        error: Box<dyn std::error::Error + Send + Sync>,

        /// The location of the error.
        #[label("field")]
        span: SourceSpan,
    },

    /// The named field failed to parse.
    #[error("field '{field}' missing from input '{input}': {error}")]
    Field {
        /// The field that failed.
        field: String,

        /// The input originally provided.
        #[source_code]
        input: String,

        /// The error encountered while parsing.
        #[source]
        error: Box<dyn std::error::Error + Send + Sync>,

        /// The location of the error.
        #[label("field")]
        span: SourceSpan,
    },
}

impl ParseError {
    /// Create a new `LiteralOneOf` variant with the provided input and options.
    pub(crate) fn new_literal_exact(input: impl Into<String>, expected: impl Into<String>) -> Self {
        let input = input.into();
        Self::LiteralExact {
            expected: expected.into(),
            span: (0, input.len()).into(),
            input,
        }
    }

    /// Create a new `LiteralOneOf` variant with the provided input and options.
    pub(crate) fn new_literal_oneof(
        input: impl Into<String>,
        options: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        let input = input.into();
        Self::LiteralOneOf {
            options: options.into_iter().map(Into::into).collect(),
            span: (0, input.len()).into(),
            input,
        }
    }
}

/// Return the span of `substr` inside `text`.
pub fn span(text: &str, substr: &str) -> (usize, usize) {
    text.find(substr)
        .map(|start| (start, substr.len() + start))
        .unwrap_or((0, text.len()))
}

/// Errors encountered when parsing a [`Constraint`](crate::Constraint) from a string.
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum ConstraintParseError {
    /// The constraint is not valid semver.
    #[error("invalid semver constraint: {0}")]
    InvalidSemver(#[from] semver::Error),

    /// An unhandled semver operator was encountered.
    #[error("unhandled semver operator: {0:?}")]
    UnhandledSemverOperator(semver::Op),
}

/// Construct and return a new [`ParseError::Source`].
///
/// Provide the input, along with the part of it that failed, and the error.
///
/// ```ignore
/// error::syntax!(input => (0, 2), err);
/// ```
macro_rules! syntax {
    ($input:expr => $span:expr, $error:expr) => {
        ParseError::Syntax {
            input: $input.into(),
            span: $span.into(),
            error: $error.into(),
        }
    };
}
pub(crate) use syntax;

/// Construct and return a new [`ParseError::Field`].
///
/// Provide the input, along with the part of it that failed, and the error.
///
/// ```ignore
/// error::field!(input, "field_name" => (0, 2), err);
/// ```
macro_rules! field {
    ($input:expr, $field:expr => $span:expr, $error:expr) => {
        ParseError::Field {
            input: $input.into(),
            field: $field.into(),
            span: $span.into(),
            error: $error.into(),
        }
    };
}
pub(crate) use field;

/// Shorthand for conversion into [`Error`] and returning.
macro_rules! fatal {
    ($err:expr) => {
        return Err($crate::error::Error::from($err))
    };
}
pub(crate) use fatal;
