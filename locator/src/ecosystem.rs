//! Identifies supported code host ecosystems.
//!
//! Packages have names and versions, like `lodash@1.0.0` or `sqlx@0.8.5`.
//! But these names and versions are only fully specified inside of the context of an ecosystem-
//! for example when we say `sqlx`, do we mean `https://github.com/launchbadge/sqlx` or `https://github.com/jmoiron/sqlx`?
//! The ecosystems in this module help us disambiguate this.
//!
//! On top of this, unfortunately FOSSA also has "ecosystems" that are private to FOSSA and have no meaning
//! outside of the context of FOSSA; these are categorized according to the below options.
//!
//! ## [`EcosystemPublic`]
//!
//! Most listed ecosystems are public ecosystems.
//!
//! For example:
//! - `Npm` implies "uses the NPM ecosystem", meaning the code referenced is distributed with the NPM package manager.
//! - `Git` implies "uses the git ecosystem", meaning the code referenced is distributed with a git server.
//!
//! ## [`EcosystemPrivate`]
//!
//! Most ecosystems are "public", meaning they're not FOSSA controlled.
//! However, some ecosystems are FOSSA-specific; these mean nothing
//! outside of the context of FOSSA.
//!
//! For example:
//! - `Archive` is an indicator for an `archive` project in FOSSA, which is a blob of uploaded source code.
//! - `Custom` is used for top-level projects in FOSSA (not all top-level projects use custom, but custom always means this).
