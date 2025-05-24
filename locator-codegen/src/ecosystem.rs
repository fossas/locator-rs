//! Types and helpers for the `ecosystem` macro.

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
    /// ```
    fn struct_variant(&self) -> TokenStream {
        let name = &self.name;
        let serialized = &self.serialized;
        let docs = &self.docs;
        let derives = Invocation::struct_derives();
        quote! {
            #(#docs)*
            #derives
            #[serde(rename = #serialized)]
            pub struct #name
        }
    }

    /// Creates the variant in an enum for this entry.
    ///
    /// ```ignore
    /// /// The `#name` ecosystem.
    /// #[strum(serialize = #serialized)]
    /// #[serde(rename = #serialized)]
    /// #name
    /// ```
    fn enum_variant(&self) -> TokenStream {
        let name = &self.name;
        let serialized = &self.serialized;
        let docs = &self.docs;
        quote! {
            #(#docs)*
            #[strum(serialize = #serialized)]
            #[serde(rename = #serialized)]
            #name
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
        let variants = self.entries.iter().map(|entry| entry.enum_variant());

        quote! {
            /// Identifies supported code host ecosystems.
            /// See module-level docs for more details on this concept.
            #derives
            #[non_exhaustive]
            pub enum #ident {
                #(#variants),*,
            }
            impl #ident {
                /// Iterate over all variants.
                pub fn iter() -> impl Iterator<Item = #ident> {
                    <Self as ::strum::IntoEnumIterator>::iter()
                }
            }
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
            #(#variants);*;
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
            let matches_to = entries
                .iter()
                .map(|entry| entry.enum_match_fragment(&category, &ecosystem));
            let matches_from = entries
                .iter()
                .map(|entry| entry.enum_match_fragment(&ecosystem, &category));
            let variants = entries.iter().map(|entry| entry.enum_variant());
            let docstr = format!("Identifies `{category}` supported code host ecosystems.");
            quote! {
                #[doc = #docstr]
                #[doc = "See module-level docs for more details on this concept."]
                #derives
                #[non_exhaustive]
                pub enum #category {
                    #(#variants),*,
                }
                impl #category {
                    /// Iterate over all variants.
                    pub fn iter() -> impl Iterator<Item = #category> {
                        <Self as ::strum::IntoEnumIterator>::iter()
                    }
                }
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
                ::serde::Serialize,
                ::serde::Deserialize,
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
                ::strum::EnumString,
                ::strum::EnumIter,
                ::strum::AsRefStr,
                ::strum::Display,
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
