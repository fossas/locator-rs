use thiserror::Error;

/// Records all errors reported by this library.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum Error {
    /// Errors encountered while parsing a [`Locator`](crate::Locator).
    #[error(transparent)]
    Parse(#[from] ParseError),
}

/// Errors encountered when parsing a [`Locator`](crate::Locator) from a string.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum ParseError {
    /// The input did not match the required syntax.
    #[error("input did not match required syntax: {input}")]
    Syntax {
        /// The input originally provided to the parser.
        input: String,
    },

    /// The "named" field was missing from the input.
    #[error("field '{field}' missing from input: {input}")]
    Field {
        /// The input originally provided to the parser.
        input: String,

        /// The field that was missing.
        field: String,
    },

    /// An unsupported value for the "fetcher" field was provided.
    /// Often this means that it is simply missing from this package.
    #[error("invalid fetcher '{fetcher}' in input '{input}'")]
    Fetcher {
        /// The input originally provided to the parser.
        input: String,

        /// The fetcher that was attempted to parse.
        fetcher: String,

        /// The error returned by the parser.
        #[source]
        error: strum::ParseError,
    },

    /// An unsupported value for the "project" field was provided.
    #[error("invalid project '{project}' in input '{input}'")]
    Project {
        /// The input originally provided to the parser.
        input: String,

        /// The project that was attempted to parse.
        project: String,

        /// The error returned by the parser.
        #[source]
        error: ProjectParseError,
    },
}

/// Errors encountered when parsing the project field
/// when parsing a [`Locator`](crate::Locator) from a string.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum ProjectParseError {
    /// An unsupported value for the "project" field was provided.
    #[error("project did not match required syntax: {project}")]
    Project {
        /// The project input.
        project: String,
    },

    /// The "named" field was missing from the input.
    #[error("field '{field}' missing from input: {project}")]
    Field {
        /// The input originally provided to the parser.
        project: String,

        /// The field that was missing.
        field: String,
    },
}
