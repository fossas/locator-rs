use thiserror::Error;

use crate::{Fetcher, Revision, constraint::Constraint};

/// Records all errors reported by this library.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum Error {
    /// Errors encountered while parsing a [`Locator`](crate::Locator).
    #[error(transparent)]
    Parse(#[from] ParseError),

    /// Errors encountered while comparing against a constraint.
    #[error(transparent)]
    Compare(#[from] CompareError),
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

    /// An unsupported value for the "package" field was provided.
    #[error("invalid package '{package}' in input '{input}'")]
    Package {
        /// The input originally provided to the parser.
        input: String,

        /// The package that was attempted to parse.
        package: String,

        /// The error returned by the parser.
        #[source]
        error: PackageParseError,
    },
}

/// Errors encountered when parsing the package field
/// when parsing a [`Locator`](crate::Locator) from a string.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum PackageParseError {
    /// An unsupported value for the "package" field was provided.
    #[error("package did not match required syntax: {package}")]
    Package {
        /// The package input.
        package: String,
    },

    /// The "named" field was missing from the input.
    #[error("field '{field}' missing from input: {package}")]
    Field {
        /// The input originally provided to the parser.
        package: String,

        /// The field that was missing.
        field: String,
    },
}

/// Errors encountered when parsing a [`Revision`](crate::Revision) from a string.
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum RevisionParseError {
    // No possible errors yet, but I'm sure there will be.
}

/// Errors encountered when comparing a [`Revision`](crate::Revision) against
/// a [`Constraint`](crate::Constraint).
#[derive(Error, Clone, PartialEq, Eq, Debug)]
#[non_exhaustive]
pub enum CompareError {
    /// The comparison is not supported.
    #[error("unsupported: constrain {comparing:#?} to {constraint:#?} using fetcher '{fetcher}'")]
    Unsupported {
        /// The fetcher that was attempted to parse.
        fetcher: Fetcher,

        /// The revision being compared.
        constraint: Constraint,

        /// The revision being compared.
        comparing: Revision,
    },

    /// Parsing the comparing revision according to the rules of the fetcher failed.
    #[error("parse {comparing:#?} according to the rules of '{fetcher}': {source}")]
    ParseComparing {
        /// The fetcher that was attempted to parse.
        fetcher: Fetcher,

        /// The revision being compared.
        comparing: Revision,

        /// The cause of the error.
        source: RevisionParseError,
    },

    /// Parsing the constraint revision according to the rules of the fetcher failed.
    #[error("parse {constraint:#?} according to the rules of '{fetcher}': {source}")]
    ParseConstraint {
        /// The fetcher that was attempted to parse.
        fetcher: Fetcher,

        /// The revision being compared.
        constraint: Constraint,

        /// The cause of the error.
        source: RevisionParseError,
    },
}

impl CompareError {
    /// Create a [`CompareError::Unsupported`] error.
    pub fn unsupported(
        fetcher: Fetcher,
        constraint: impl Into<Constraint>,
        comparing: impl Into<Revision>,
    ) -> Self {
        Self::Unsupported {
            fetcher,
            comparing: comparing.into(),
            constraint: constraint.into(),
        }
    }

    /// Create a [`CompareError::ParseComparing`] error.
    pub fn parse_comparing(
        fetcher: Fetcher,
        comparing: impl Into<Revision>,
        source: RevisionParseError,
    ) -> Self {
        Self::ParseComparing {
            fetcher,
            comparing: comparing.into(),
            source,
        }
    }

    /// Create a [`CompareError::ParseConstraint`] error.
    pub fn parse_constraint(
        fetcher: Fetcher,
        constraint: impl Into<Constraint>,
        source: RevisionParseError,
    ) -> Self {
        Self::ParseConstraint {
            fetcher,
            constraint: constraint.into(),
            source,
        }
    }
}
