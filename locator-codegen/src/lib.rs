//! Codegen macros for the `locator` crate.

use ecosystem::Invocation;
use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemStruct, parse_macro_input};

mod ecosystem;
mod locator_parts;

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

/// Generate a newtype wrapper around `LocatorParts` with customizable getter implementations.
///
/// This attribute macro creates a newtype that wraps `LocatorParts<E, O, P, R>` and generates:
/// 1. A builder pattern using `bon::bon`
/// 2. Getter methods with default or custom implementations
/// 3. Standard trait derivations that pass through to the inner `LocatorParts`
///
/// # Usage
///
/// The macro takes:
/// - `types(E, O, P, R)`: The generic type parameters for the underlying `LocatorParts`
/// - `get(field_name = fn ...)`: Optional custom getter implementations
///
/// Call it before your derives on a unit struct with your desired name and visibility:
/// ```ignore
/// #[locator_parts(
///     types(Ecosystem, Option<OrgId>, Package, Option<Revision>),
/// )]
/// #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize, Display, ToSchema)]
/// #[schema(value_type = String, example = json!("npm+lodash$1.0.0"))]
/// pub struct Locator;
/// ```
///
/// This expands to something like:
/// ```ignore
/// #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize, Display, ToSchema)]
/// #[schema(value_type = String, example = json!("npm+lodash$1.0.0"))]
/// pub struct Locator(LocatorParts<Ecosystem, Option<OrgId>, Package, Option<Revision>>);
/// ```
///
/// Any other attributes you provide, such as derives or documentation, will attach to the generated newtype;
/// just make sure they take into account the actual underlying type.
///
/// ## Default Behavior
///
/// By default, all getters return references to the inner fields:
/// ```ignore
/// pub fn ecosystem(&self) -> &E { &self.0.ecosystem }
/// pub fn organization(&self) -> &O { &self.0.organization }
/// pub fn package(&self) -> &P { &self.0.package }
/// pub fn revision(&self) -> &R { &self.0.revision }
/// ```
///
/// ## Custom Getters
///
/// You can override specific getters by providing custom implementations:
/// ```ignore
/// #[locator_parts(
///     types(Ecosystem, Option<OrgId>, Package, Option<Revision>),
///     get(ecosystem = fn ecosystem(&self) -> Ecosystem { self.0.ecosystem }),
///     get(organization = fn organization(&self) -> Option<OrgId> { self.0.organization }),
///     get(revision = fn revision(&self) -> Option<&Revision> { self.0.revision.as_ref() })
/// )]
/// #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize, Display)]
/// pub struct Locator;
/// ```
///
/// This expands to something like:
/// ```ignore
/// #[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize, Display)]
/// pub struct Locator(LocatorParts<Ecosystem, Option<OrgId>, Package, Option<Revision>>);
///
/// #[bon::bon]
/// impl Locator {
///     #[builder]
///     pub fn builder(
///         #[builder(into)] ecosystem: Ecosystem,
///         #[builder(into)] organization: Option<OrgId>,
///         #[builder(into)] package: Package,
///         #[builder(into)] revision: Option<Revision>,
///     ) -> Self {
///         Self(LocatorParts::new(ecosystem, organization, package, revision))
///     }
/// }
///
/// impl Locator {
///     pub fn ecosystem(&self) -> Ecosystem {
///         self.0.ecosystem
///     }
///
///     pub fn organization(&self) -> Option<OrgId> {
///         self.0.organization
///     }
///
///     pub fn package(&self) -> &Package {
///         &self.0.package
///     }
///
///     pub fn revision(&self) -> Option<&Revision> {
///         self.0.revision.as_ref()
///     }
/// }
/// ```
///
/// Note that this intentionally does not allow for overriding the field documentation.
///
/// ## Requirements
///
/// The `locator` crate must be available in scope.
#[proc_macro_attribute]
pub fn locator_parts(attr: TokenStream, item: TokenStream) -> TokenStream {
    let invocation = parse_macro_input!(attr as locator_parts::Invocation);
    let input_struct = parse_macro_input!(item as ItemStruct);

    let struct_name = &input_struct.ident;
    let attrs = &input_struct.attrs;

    let generated = invocation.generate_impl(struct_name, attrs);

    TokenStream::from(generated)
}

/// Generate a string literal to be used docs for the `ecosystem` field on a `LocatorParts`-like type.
///
/// Since this can't be used as a field attribute directly, use it as a proc macro:
/// ```ignore
/// #[doc = locator_codegen::field_doc_ecosystem!()]
/// pub ecosystem: E,
/// ```
#[proc_macro]
pub fn field_doc_ecosystem(_: TokenStream) -> TokenStream {
    let doc = locator_parts::field_doc_ecosystem();
    TokenStream::from(quote! {
        #doc
    })
}

/// Generate a string literal to be used docs for the `organization` field on a `LocatorParts`-like type.
///
/// Since this can't be used as a field attribute directly, use it as a proc macro:
/// ```ignore
/// #[doc = locator_codegen::field_doc_organization!()]
/// pub organization: O,
/// ```
#[proc_macro]
pub fn field_doc_organization(_: TokenStream) -> TokenStream {
    TokenStream::from(locator_parts::field_doc_organization())
}

/// Generate a string literal to be used docs for the `package` field on a `LocatorParts`-like type.
///
/// Since this can't be used as a field attribute directly, use it as a proc macro:
/// ```ignore
/// #[doc = locator_codegen::field_doc_package!()]
/// pub package: P,
/// ```
#[proc_macro]
pub fn field_doc_package(_: TokenStream) -> TokenStream {
    TokenStream::from(locator_parts::field_doc_package())
}

/// Generate a string literal to be used docs for the `revision` field on a `LocatorParts`-like type.
///
/// Since this can't be used as a field attribute directly, use it as a proc macro:
/// ```ignore
/// #[doc = locator_codegen::field_doc_revision!()]
/// pub revision: R,
/// ```
#[proc_macro]
pub fn field_doc_revision(_: TokenStream) -> TokenStream {
    TokenStream::from(locator_parts::field_doc_revision())
}
