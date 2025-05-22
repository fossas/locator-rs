//! Codegen macros for the `locator` crate.

use ecosystem::Invocation;
use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemStruct, parse_macro_input};

mod ecosystem;

/// Generate ecosystem types based on the provided definition.
///
/// This macro creates a module that contains:
/// 1. A main `Ecosystem` enum containing all ecosystem variants
/// 2. Category-specific enums (subsets of the main enum)
/// 3. Individual structs for each ecosystem
/// 4. Conversion implementations between all types
///
/// # Usage
///
///
/// The format for each variant is `Category => Variant, "serialization";`.
/// You should write doc comments above each variant, which are then propagated to everything generated from that variant.
///
/// Finally, attach the macro invocation to a unit struct.
/// The macro converts the unit struct to a `mod` with the same visibility and name.
/// See "why is this attribute attached to a unit struct?" for details.
///
/// Example:
/// ```ignore
/// /// Write documentation for the generated `ecosystems` module here.
/// #[ecosystems(
///     /// Write documentation for the variant here.
///     Private => VariantOne, "variant_one";
///     /// Write documentation for the variant here.
///     /// This can span multiple lines.
///     Public => VariantTwo, "variant_two";
/// )]
/// pub struct ecosystems;
/// ```
///
/// This isn't a fully spelled out expansion as there are other various expansions
/// such as derive invocations and iterators and such, but the above expands to something like this:
/// ```ignore
/// /// Write documentation for the generated `ecosystems` module here.
/// pub mod ecosystems {
///     /// Indicates the conversion requested is not valid.
///     #[derive(Debug, ::thiserror::Error)]
///     #[error("invalid conversion")]
///     pub struct InvalidConversionError;
///
///     /// Identifies supported code host ecosystems.
///     /// See module-level docs for more details on this concept.
///     #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, ...)]
///     #[non_exhaustive]
///     pub enum Ecosystem {
///         /// Write documentation for the variant here.
///         #[strum(serialize = "variant_one")]
///         #[serde(rename = "variant_one")]
///         VariantOne,
///
///         /// Write documentation for the variant here.
///         /// This can span multiple lines.
///         #[strum(serialize = "variant_two")]
///         #[serde(rename = "variant_two")]
///         VariantTwo,
///     }
///
///     /// Identifies `Private` supported code host ecosystems.
///     /// See module-level docs for more details on this concept.
///     #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, ...)]
///     #[non_exhaustive]
///     pub enum EcosystemPrivate {
///         /// Write documentation for the variant here.
///         #[strum(serialize = "variant_one")]
///         #[serde(rename = "variant_one")]
///         VariantOne,
///     }
///
///     /// Identifies `Public` supported code host ecosystems.
///     /// See module-level docs for more details on this concept.
///     #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, ...)]
///     #[non_exhaustive]
///     pub enum EcosystemPublic {
///         /// Write documentation for the variant here.
///         /// This can span multiple lines.
///         #[strum(serialize = "variant_two")]
///         #[serde(rename = "variant_two")]
///         VariantTwo,
///     }
///
///     impl From<EcosystemPublic> for Ecosystem {
///         fn from(value: EcosystemPublic) -> Self {
///             match value {
///                 EcosystemPublic::VariantTwo => Ecosystem::VariantTwo,
///             }
///         }
///     }
///
///     impl From<EcosystemPrivate> for Ecosystem {
///         fn from(value: EcosystemPublic) -> Self {
///             match value {
///                 EcosystemPublic::VariantOne => Ecosystem::VariantOne,
///             }
///         }
///     }
///
///     impl TryFrom<Ecosystem> for EcosystemPublic {
///         type Err = InvalidConversionError;
///         fn try_from(value: Ecosystem) -> Result<Self, InvalidConversionError> {
///             Ok(match value {
///                 Ecosystem::VariantTwo => EcosystemPublic::VariantTwo,
///                 _ => return Err(InvalidConversionError);
///             })
///         }
///     }
///
///     impl TryFrom<Ecosystem> for EcosystemPrivate {
///         type Err = InvalidConversionError;
///         fn try_from(value: Ecosystem) -> Result<Self, InvalidConversionError> {
///             Ok(match value {
///                 Ecosystem::VariantOne => EcosystemPrivate::VariantOne,
///                 _ => return Err(InvalidConversionError);
///             })
///         }
///     }
///
///     /// Write documentation for the variant here.
///     #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, ...)]
///     #[strum(serialize = "variant_one")]
///     #[serde(rename = "variant_one")]
///     pub struct VariantOne;
///
///     impl From<VariantOne> for Ecosystem {
///         fn from(_: VariantOne) -> Self {
///             Ecosystem::VariantOne
///         }
///     }
///
///     impl From<VariantOne> for EcosystemPrivate {
///         fn from(_: VariantOne) -> Self {
///             EcosystemPrivate::VariantOne
///         }
///     }
///
///     impl TryFrom<Ecosystem> for VariantOne {
///         type Err = InvalidConversionError;
///         fn try_from(value: Ecosystem) -> Result<Self, InvalidConversionError> {
///             Ok(match value {
///                 Ecosystem::VariantOne => VariantOne,
///                 _ => return Err(InvalidConversionError);
///             })
///         }
///     }
///
///     impl TryFrom<EcosystemPrivate> for VariantOne {
///         type Err = InvalidConversionError;
///         fn try_from(value: EcosystemPrivate) -> Result<Self, InvalidConversionError> {
///             Ok(match value {
///                 EcosystemPrivate::VariantOne => VariantOne,
///                 _ => return Err(InvalidConversionError);
///             })
///         }
///     }
///
///     /// Write documentation for the variant here.
///     /// This can span multiple lines.
///     #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd, ...)]
///     #[strum(serialize = "variant_two")]
///     #[serde(rename = "variant_two")]
///     pub struct VariantTwo;
///
///     impl From<VariantTwo> for Ecosystem {
///         fn from(_: VariantTwo) -> Self {
///             Ecosystem::VariantTwo
///         }
///     }
///
///     impl From<VariantTwo> for EcosystemPublic {
///         fn from(_: VariantTwo) -> Self {
///             EcosystemPublic::VariantTwo
///         }
///     }
///
///     impl TryFrom<Ecosystem> for VariantTwo {
///         type Err = InvalidConversionError;
///         fn try_from(value: Ecosystem) -> Result<Self, InvalidConversionError> {
///             Ok(match value {
///                 Ecosystem::VariantTwo => VariantTwo,
///                 _ => return Err(InvalidConversionError);
///             })
///         }
///     }
///
///     impl TryFrom<EcosystemPublic> for VariantTwo {
///         type Err = InvalidConversionError;
///         fn try_from(value: EcosystemPublic) -> Result<Self, InvalidConversionError> {
///             Ok(match value {
///                 EcosystemPublic::VariantTwo => VariantTwo,
///                 _ => return Err(InvalidConversionError);
///             })
///         }
///     }
/// }
/// ```
///
/// # Generated Types
///
/// * Main `Ecosystem` enum with all provided variants
/// * Sub-category enums for each category specified
/// * Individual structs: One per variant (e.g., `Archive`, `Cargo`)
/// * Error type: `InvalidConversionError` for failed conversions
///
/// # Conversions
///
/// The generated code provides conversions between:
/// * Individual structs and the main `Ecosystem` enum
/// * Individual structs and their category enum
/// * Category enums and the main `Ecosystem` enum
///
/// Implements `TryFrom` with all fallible conversions, `From` otherwise.
/// Conversions are fallible if they are converting from a superset to a subset,
/// and infallible in the reverse case.
///
/// In other words, in the example above we know at compile time that `VariantOne` is a variant of both
/// `Ecosystem` and `EcosystemPrivate`, but we can't know that a given `Ecosystem` or `EcosystemPrivate` variant
/// is specifically `VariantOne` at compile time. Same deal with the category enums- their variants can always
/// convert to the `Ecosystem` enum, but not necessarily the inverse.
///
/// # Why is this attribute attached to a unit struct?
///
/// This is a stable Rust limitation.
///
/// I want it to create a module, but can't attach an attribute macro to a statement like `pub mod ecosystems;`
/// because that implies a module in a different file (which doesn't exist).
///
/// Meanwhile I can't just use a function-like macro because then it can't
/// capture the doc comments attached to the macro invocation.
///
/// So the least bad alternative is to attach the macro invocation to a unit struct,
/// and then silently translate that into a module instead. Alas.
#[proc_macro_attribute]
pub fn ecosystems(attr: TokenStream, item: TokenStream) -> TokenStream {
    let invocation = parse_macro_input!(attr as ecosystem::Invocation);
    let module = parse_macro_input!(item as ItemStruct);

    let vis = &module.vis;
    let name = &module.ident;
    let docs = module
        .attrs
        .into_iter()
        .filter(|a| a.path().is_ident("doc"))
        .collect::<Vec<_>>();

    let invalid_conversion_err = Invocation::mk_invalid_conversion_error();
    let ecosystems = invocation.mk_ecosystem_enum();
    let subsets = invocation.mk_ecosystem_category_enums();
    let structs = invocation.mk_ecosystem_structs();

    TokenStream::from(quote! {
        #(#docs)*
        #vis mod #name {
            #invalid_conversion_err;
            #ecosystems
            #subsets
            #structs
        }
    })
}
