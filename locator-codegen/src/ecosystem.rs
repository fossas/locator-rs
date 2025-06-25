//! Types and helpers for the `ecosystem` macro.
//! Note that this macro is not meant to be invoked from outside the `locator` crate.

use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream, Result};
use syn::{Attribute, Ident, LitStr, Token};

/// An entry provided by the user.
#[derive(Debug, Clone)]
pub struct Entry {
    /// Docs on the entry.
    docs: Vec<Attribute>,

    /// Categorizes the entry into a subset of the overall set of ecosystems.
    category: Ident,

    /// The name of this entry.
    name: Ident,

    /// How this entry is serialized.
    serialized: LitStr,
}

impl Entry {
    /// Creates a standalone struct for this variant.
    ///
    /// ```ignore
    /// /// The `#name` ecosystem.
    /// #derives
    /// pub struct #name;
    ///
    /// impl #name {
    ///     /// Parse an instance from a string.
    ///     pub fn parse(input: impl AsRef<str>) -> Result<Self, locator::Error>;
    ///
    ///     /// View the instance as a string.
    ///     pub fn as_str(&self) -> &str {
    /// }
    /// ```
    fn struct_variant(&self) -> TokenStream {
        let name = &self.name;
        let serialized = &self.serialized;
        let docs = &self.docs;
        let derives = Invocation::struct_derives();
        let fromstrings = mk_tryfromstr_parse(&name);
        let displays = mk_display_from_str(&name);
        let deserialize = mk_deserialize_parse(&name);
        let serialize = mk_serialize_from_str(&name);
        quote! {
            #(#docs)*
            #derives
            pub struct #name;
            impl #name {
                /// Parse an instance from a string.
                pub fn parse(input: impl AsRef<str>) -> Result<Self, locator::Error> {
                    let input = input.as_ref();
                    if input == #serialized {
                        Ok(#name)
                    } else {
                        Err(locator::ParseError::new_literal_exact(input, #serialized).into())
                    }
                }
                /// View the instance as a string.
                pub fn as_str(&self) -> &str {
                    #serialized
                }
            }
            #fromstrings
            #displays
            #deserialize
            #serialize
        }
    }

    /// Creates the variant in an enum for this entry.
    ///
    /// ```ignore
    /// /// The `#name` ecosystem.
    /// #[serde(rename = #serialized)]
    /// #name
    /// ```
    fn enum_variant(&self) -> TokenStream {
        let name = &self.name;
        let serialized = &self.serialized;
        let docs = &self.docs;
        quote! {
            #(#docs)*
            #[serde(rename = #serialized)]
            #name
        }
    }

    /// Creates the enum variant for serializing this entry.
    ///
    /// ```ignore
    /// #name => #serialized
    /// ```
    fn enum_variant_ser(&self, parent: &Ident) -> TokenStream {
        let variant = &self.name;
        let serialized = &self.serialized;
        quote! {
            #parent::#variant => #serialized
        }
    }

    /// Creates the enum variant for deserializing this entry.
    ///
    /// ```ignore
    /// #serialized => #name
    /// ```
    fn enum_variant_de(&self, parent: &Ident) -> TokenStream {
        let variant = &self.name;
        let serialized = &self.serialized;
        quote! {
            #serialized => Ok(#parent::#variant)
        }
    }

    /// Create an enum match fragment matching the name of this entry
    /// from one enum ident to another.
    ///
    /// ```ignore
    /// #from::#name => #to::#name
    /// ```
    fn enum_match_fragment(&self, from: &Ident, to: &Ident) -> TokenStream {
        let name = &self.name;
        quote! { #from::#name => #to::#name }
    }
}

/// Parsed invocation of the `ecosystems` macro.
#[derive(Debug, Clone)]
pub struct Invocation {
    /// The parsed entries from the invocation.
    pub entries: Vec<Entry>,
}

impl Invocation {
    /// Construct the `InvalidConversionError`.
    pub fn mk_invalid_conversion_error() -> TokenStream {
        let name = Self::invalid_conversion();
        quote! {
            /// Indicates the conversion requested is not valid.
            #[derive(Debug, ::thiserror::Error)]
            #[error("invalid conversion")]
            pub struct #name
        }
    }

    /// Construct the main `Ecosystems` enum.
    pub fn mk_ecosystem_enum(&self) -> TokenStream {
        let ident = Self::ecosystem();
        let derives = Self::enum_derives();
        let entries = &self.entries;
        let variants = entries.iter().map(|entry| entry.enum_variant());
        let names = entries.iter().map(|entry| &entry.name);

        let options = entries.iter().map(|entry| &entry.serialized);
        let variants_serialized = entries.iter().map(|e| e.enum_variant_ser(&ident));
        let variants_deserialized = entries.iter().map(|e| e.enum_variant_de(&ident));
        let fromstrings = mk_tryfromstr_parse(&ident);
        let displays = mk_display_from_str(&ident);

        quote! {
            /// Identifies supported code host ecosystems.
            /// See module-level docs for more details on this concept.
            #derives
            #[non_exhaustive]
            pub enum #ident {
                #(#variants),*,
            }
            impl #ident {
                /// Parse an instance from a string.
                pub fn parse(input: impl AsRef<str>) -> Result<Self, locator::Error> {
                    let input = input.as_ref();
                    match input {
                        #(#variants_deserialized),*,
                        _ => Err(locator::ParseError::new_literal_oneof(input, [#(#options),*]).into()),
                    }
                }
                /// View the instance as a string.
                pub fn as_str(&self) -> &str {
                    match self {
                        #(#variants_serialized),*
                    }
                }
                /// Iterate over all variants.
                pub fn iter() -> impl Iterator<Item = #ident> {
                    [#(#ident::#names),*].into_iter()
                }
            }
            #fromstrings
            #displays
            impl From<&#ident> for #ident {
                fn from(value: &#ident) -> Self {
                    *value
                }
            }
        }
    }

    /// Construct the ecosystem structs.
    pub fn mk_ecosystem_structs(&self) -> TokenStream {
        let ecosystem = Self::ecosystem();
        let invalid_conversion = Self::invalid_conversion();

        let categories = self.category_entries();
        let variants = self.entries.iter().map(|e| e.struct_variant());
        let conversions = self.entries.iter().map(|entry| {
            let name = &entry.name;
            quote! {
                impl TryFrom<#ecosystem> for #name {
                    type Error = #invalid_conversion;
                    fn try_from(value: #ecosystem) -> Result<Self, Self::Error> {
                        match value {
                            #ecosystem::#name => Ok(#name),
                            _ => Err(#invalid_conversion),
                        }
                    }
                }
                impl From<#name> for #ecosystem {
                    fn from(value: #name) -> Self {
                        #ecosystem::#name
                    }
                }
                impl From<&#name> for #name {
                    fn from(value: &#name) -> Self {
                        *value
                    }
                }
            }
        });

        // We know all of the variant structs have already been constructed;
        // this is just for conversions with category specific enums.
        let variant_conversions = categories.iter().map(|(category, entries)| {
            let category = Self::ecosystem_category(category);
            let matches_to = entries.iter().map(|entry| {
                let name = &entry.name;
                quote! {
                    impl From<#name> for #category {
                        fn from(_: #name) -> Self {
                            #category::#name
                        }
                    }
                }
            });
            let matches_from = entries.iter().map(|entry| {
                let name = &entry.name;
                quote! {
                    impl TryFrom<#category> for #name {
                        type Error = #invalid_conversion;
                        fn try_from(value: #category) -> Result<Self, Self::Error> {
                            match value {
                                #category::#name => Ok(#name),
                                _ => Err(#invalid_conversion),
                            }
                        }
                    }
                }
            });
            quote! {
                #(#matches_to)*
                #(#matches_from)*
            }
        });

        quote! {
            #(#variants)*
            #(#conversions)*
            #(#variant_conversions)*
        }
    }

    /// Construct the subset enums.
    pub fn mk_ecosystem_category_enums(&self) -> TokenStream {
        let derives = Self::enum_derives();
        let ecosystem = Self::ecosystem();
        let invalid_conversion = Self::invalid_conversion();

        let categories = self.category_entries();
        let enums = categories.iter().map(|(category, entries)| {
            let category = Self::ecosystem_category(category);
            let matches_to = entries.iter().map(|entry| entry.enum_match_fragment(&category, &ecosystem));
            let matches_from = entries.iter().map(|entry| entry.enum_match_fragment(&ecosystem, &category));
            let variants = entries.iter().map(|entry| entry.enum_variant());
            let names = entries.iter().map(|entry| &entry.name);
            let options = entries.iter().map(|entry| &entry.serialized);
            let variants_serialized = entries.iter().map(|e| e.enum_variant_ser(&category));
            let variants_deserialized = entries.iter().map(|e| e.enum_variant_de(&category));
            let fromstrings = mk_tryfromstr_parse(&category);
            let displays = mk_display_from_str(&category);
            let doc = format!("Identifies `{category}` supported code host ecosystems.");
            quote! {
                #[doc = #doc]
                #[doc = "See module-level docs for more details on this concept."]
                #derives
                #[non_exhaustive]
                pub enum #category {
                    #(#variants),*,
                }
                impl #category {
                    /// Parse an instance from a string.
                    pub fn parse(input: impl AsRef<str>) -> Result<Self, locator::Error> {
                        let input = input.as_ref();
                        match input {
                            #(#variants_deserialized),*,
                            _ => Err(locator::ParseError::new_literal_oneof(input, [#(#options),*]).into()),
                        }
                    }
                    /// View the instance as a string.
                    pub fn as_str(&self) -> &str {
                        match self {
                            #(#variants_serialized),*
                        }
                    }
                    /// Iterate over all variants.
                    pub fn iter() -> impl Iterator<Item = #category> {
                        [#(#category::#names),*].into_iter()
                    }
                }
                #fromstrings
                #displays
                impl From<#category> for #ecosystem {
                    fn from(value: #category) -> Self {
                        match value {
                            #(#matches_to),*,
                        }
                    }
                }
                impl TryFrom<#ecosystem> for #category {
                    type Error = #invalid_conversion;
                    fn try_from(value: #ecosystem) -> Result<Self, Self::Error> {
                        Ok(match value {
                            #(#matches_from),*,
                            _ => return Err(#invalid_conversion),
                        })
                    }
                }
                impl From<&#category> for #category {
                    fn from(value: &#category) -> Self {
                        *value
                    }
                }
            }
        });

        quote! {
            #(#enums)*
        }
    }

    /// The ident of the main `Ecosystem` enum.
    ///
    /// ```ignore
    /// Ecosystem
    /// ```
    fn ecosystem() -> Ident {
        Ident::new("Ecosystem", proc_macro2::Span::call_site())
    }

    /// The ident of an ecosystem category enum.
    ///
    /// ```ignore
    /// Ecosystem#category
    /// ```
    fn ecosystem_category(category: &Ident) -> Ident {
        format_ident!("Ecosystem{category}")
    }

    /// The ident of the `InvalidConversionError` error.
    ///
    /// ```ignore
    /// InvalidConversionError
    /// ```
    fn invalid_conversion() -> Ident {
        Ident::new("InvalidConversionError", proc_macro2::Span::call_site())
    }

    /// Derives suitable for structs.
    fn struct_derives() -> TokenStream {
        quote! {
            #[derive(
                Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd,
                ::documented::Documented,
                ::utoipa::ToSchema,
            )]
        }
    }

    /// Derives suitable for enums.
    fn enum_derives() -> TokenStream {
        quote! {
            #[derive(
                Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd,
                ::serde::Serialize,
                ::serde::Deserialize,
                ::documented::Documented,
                ::utoipa::ToSchema,
            )]
        }
    }

    /// Sort all entries into categories.
    fn category_entries(&self) -> HashMap<&Ident, Vec<&Entry>> {
        self.entries.iter().fold(HashMap::new(), |mut cats, entry| {
            cats.entry(&entry.category).or_default().push(entry);
            cats
        })
    }
}

impl Parse for Invocation {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut entries = Vec::new();
        while !input.is_empty() {
            let attrs: Vec<Attribute> = input.call(Attribute::parse_outer)?;
            let docs = attrs
                .into_iter()
                .filter(|a| a.path().is_ident("doc"))
                .collect::<Vec<_>>();

            let category: Ident = input.parse()?;
            input.parse::<Token![=>]>()?;

            let name: Ident = input.parse()?;
            input.parse::<Token![,]>()?;

            let serialized: LitStr = input.parse()?;
            let _ = input.parse::<Token![;]>();

            entries.push(Entry {
                docs,
                category,
                name,
                serialized,
            });
        }
        Ok(Self { entries })
    }
}

/// Implement `Deserialize` using `Self::parse`.
fn mk_deserialize_parse(ident: &Ident) -> TokenStream {
    quote! {
        impl<'de> serde::de::Deserialize<'de> for #ident {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                let s = String::deserialize(deserializer)?;
                Self::parse(&s).map_err(serde::de::Error::custom)
            }
        }
    }
}

/// Implement `Serialize` using `Self::as_str`.
fn mk_serialize_from_str(ident: &Ident) -> TokenStream {
    quote! {
        impl serde::ser::Serialize for #ident {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::ser::Serializer,
            {
                serializer.serialize_str(self.as_str())
            }
        }
    }
}

/// Construct `TryFrom` conversions for `String`, `&String`, and `&str` using `Self::parse`.
fn mk_tryfromstr_parse(ident: &Ident) -> TokenStream {
    quote! {
        impl TryFrom<&str> for #ident {
            type Error = locator::Error;
            fn try_from(value: &str) -> Result<Self, Self::Error> {
                Self::parse(value)
            }
        }
        impl TryFrom<&String> for #ident {
            type Error = locator::Error;
            fn try_from(value: &String) -> Result<Self, Self::Error> {
                Self::parse(value)
            }
        }
        impl TryFrom<String> for #ident {
            type Error = locator::Error;
            fn try_from(value: String) -> Result<Self, Self::Error> {
                Self::parse(value)
            }
        }
    }
}

/// Construct `Display` and `AsRef<str>` conversions using `Self::as_str`.
fn mk_display_from_str(ident: &Ident) -> TokenStream {
    quote! {
        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }
        impl std::convert::AsRef<str> for #ident {
            fn as_ref(&self) -> &str {
                self.as_str()
            }
        }
    }
}
