use thiserror::Error;

/// Records all errors reported by this library.
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum Error {
    /// Errors encountered while parsing a [`Locator`](crate::Locator).
    #[error(transparent)]
    Parse(#[from] ParseError),

    /// Errors encountered while parsing a [`Constraint`](crate::Constraint).
    #[error(transparent)]
    ParseConstraint(#[from] ConstraintParseError),
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
