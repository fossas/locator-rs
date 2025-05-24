//! Types and helpers for the `locator_parts` macro.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream, Result};
use syn::{Attribute, Ident, ImplItemFn, Token, Type, punctuated::Punctuated};

/// Parsed invocation of the `locator_parts` macro.
#[derive(Clone)]
pub struct Invocation {
    /// The interior types of the LocatorParts generic.
    pub types: LocatorTypes,
    /// Custom getter implementations provided by the user.
    pub custom_getters: HashMap<String, ImplItemFn>,
}

/// The four generic type parameters for LocatorParts<E, O, P, R>.
#[derive(Clone)]
pub struct LocatorTypes {
    pub ecosystem: Type,
    pub organization: Type,
    pub package: Type,
    pub revision: Type,
}

impl Invocation {
    /// Generate the complete newtype implementation.
    pub fn generate_impl(&self, struct_name: &Ident, attrs: &[Attribute]) -> TokenStream {
        let struct_def = self.generate_struct(struct_name, attrs);
        let builder_impl = self.generate_builder_impl(struct_name);
        let getters_impl = self.generate_getters_impl(struct_name);

        quote! {
            #struct_def
            #builder_impl
            #getters_impl
        }
    }

    /// Generate the newtype struct definition.
    fn generate_struct(&self, name: &Ident, attrs: &[Attribute]) -> TokenStream {
        let LocatorTypes {
            ecosystem,
            organization,
            package,
            revision,
        } = &self.types;

        quote! {
            #(#attrs)*
            pub struct #name(
                locator::LocatorParts<#ecosystem, #organization, #package, #revision>
            );
            impl #name {
                /// Parse an instance from a string.
                /// For parsing details, see the type documentation.
                pub fn parse(input: impl AsRef<str>) -> Result<Self, locator::Error> {
                    locator::LocatorParts::parse(input).map(Self)
                }
                /// Extract the instance to its parts.
                pub(crate) fn into_parts(self) -> locator::LocatorParts<#ecosystem, #organization, #package, #revision> {
                    self.0
                }
                /// Construct an instance from its parts.
                pub(crate) fn from_parts(parts: locator::LocatorParts<#ecosystem, #organization, #package, #revision>) -> Self {
                    Self(parts)
                }
            }
            impl From<locator::LocatorParts<#ecosystem, #organization, #package, #revision>> for #name {
                fn from(parts: locator::LocatorParts<#ecosystem, #organization, #package, #revision>) -> Self {
                    Self::from_parts(parts)
                }
            }
            impl From<#name> for locator::LocatorParts<#ecosystem, #organization, #package, #revision> {
                fn from(instance: #name) -> Self {
                    instance.into_parts()
                }
            }
        }
    }

    /// Generate the builder implementation using bon::bon.
    fn generate_builder_impl(&self, struct_name: &Ident) -> TokenStream {
        let LocatorTypes {
            ecosystem,
            organization,
            package,
            revision,
        } = &self.types;

        quote! {
            #[locator::macro_support::bon::bon]
            impl #struct_name {
                /// Construct a new instance with the provided values.
                #[builder]
                pub fn builder(
                    #[builder(into)] ecosystem: #ecosystem,
                    #[builder(into)] organization: #organization,
                    #[builder(into)] package: #package,
                    #[builder(into)] revision: #revision,
                ) -> Self {
                    Self(locator::LocatorParts::new(
                        ecosystem,
                        organization,
                        package,
                        revision,
                    ))
                }
            }
        }
    }

    /// Generate the getter methods implementation.
    fn generate_getters_impl(&self, struct_name: &Ident) -> TokenStream {
        let ecosystem_getter = self.generate_getter("ecosystem", &field_doc_ecosystem());
        let organization_getter = self.generate_getter("organization", &field_doc_organization());
        let package_getter = self.generate_getter("package", &field_doc_package());
        let revision_getter = self.generate_getter("revision", &field_doc_revision());

        quote! {
            impl #struct_name {
                #ecosystem_getter
                #organization_getter
                #package_getter
                #revision_getter
            }
        }
    }

    /// Generate a getter method, using custom implementation if provided, otherwise default.
    fn generate_getter(&self, field: &str, doc: &TokenStream) -> TokenStream {
        if let Some(custom_fn) = self.custom_getters.get(field) {
            quote! {
                #[doc = #doc]
                #custom_fn
            }
        } else {
            let name = format_ident!("{field}");
            let output = match field {
                "ecosystem" => &self.types.ecosystem,
                "organization" => &self.types.organization,
                "package" => &self.types.package,
                "revision" => &self.types.revision,
                _ => panic!("internal proc-macro error: unknown field '{field}'"),
            };
            quote! {
                #[doc = #doc]
                pub fn #name(&self) -> &#output {
                    &self.0.#name
                }
            }
        }
    }
}

impl Parse for Invocation {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let types: Ident = input.parse()?;
        if types != "types" {
            return Err(syn::Error::new_spanned(types, "Expected 'types'"));
        }

        let types_content;
        syn::parenthesized!(types_content in input);
        let type_args: Punctuated<Type, Token![,]> =
            types_content.parse_terminated(Type::parse, Token![,])?;

        if type_args.len() != 4 {
            return Err(syn::Error::new_spanned(
                &type_args,
                "Expected exactly 4 types for LocatorParts<E, O, P, R>",
            ));
        }

        let types = LocatorTypes {
            ecosystem: type_args[0].clone(),
            organization: type_args[1].clone(),
            package: type_args[2].clone(),
            revision: type_args[3].clone(),
        };

        let mut custom_getters = HashMap::new();
        while !input.is_empty() {
            input.parse::<Token![,]>()?;

            if input.is_empty() {
                break;
            }

            let lookahead = input.lookahead1();
            if lookahead.peek(Ident) {
                let get: Ident = input.parse()?;
                if get != "get" {
                    return Err(syn::Error::new_spanned(get, "Expected 'get'"));
                }

                let get_content;
                syn::parenthesized!(get_content in input);

                let field_name: Ident = get_content.parse()?;
                get_content.parse::<Token![=]>()?;
                let custom_fn: ImplItemFn = get_content.parse()?;

                custom_getters.insert(field_name.to_string(), custom_fn);
            } else {
                break;
            }
        }

        Ok(Invocation {
            types,
            custom_getters,
        })
    }
}

pub fn field_doc_ecosystem() -> TokenStream {
    let doc = proc_macro2::Literal::string(
        "Determines which ecosystem is used to download this package.",
    );
    quote! {
        #doc
    }
}

pub fn field_doc_organization() -> TokenStream {
    let doc = proc_macro2::Literal::string(
        r#"
        Specifies the organization to which this package is namespaced.

        Locators are namespaced to an organization when FOSSA needs to use the
        private repositories or settings configured by the user to resolve the package.

        Generally, users can treat this as an implementation detail:
        Organization IDs namespacing a package means the package should concretely be considered different;
        for example `npm+lodash$1.0.0` should be considered different from `npm+1234/lodash$1.0.0`.
        The reasoning for this is that private packages may be totally different than
        a similarly named public package- in the example above, both of them being `lodash@1.0.0`
        doesn't really imply that they are both the popular project known as "lodash".
        We know the public one is (`npm+lodash$1.0.0`), but the private one could be anything.

        Examples:
        - A public Maven package that is hosted on Maven Central is not namespaced.
        - A private Maven package that is hosted on a private host is namespaced.
        - A public NPM package that is hosted on NPM is not namespaced.
        - A private NPM package that is hosted on NPM but requires credentials is namespaced.
        "#,
    );
    quote! {
        #doc
    }
}

pub fn field_doc_package() -> TokenStream {
    let doc = proc_macro2::Literal::string(
        r#"
        Specifies the unique identifier for the package by ecosystem.

        For example, the `git` ecosystem fetching a github package
        uses a value in the form of `{user_name}/{package_name}`.
        "#,
    );
    quote! {
        #doc
    }
}

pub fn field_doc_revision() -> TokenStream {
    let doc = proc_macro2::Literal::string(
        r#"
        Specifies the version for the package by ecosystem.

        For example, the `git` ecosystem fetching a github package
        uses a value in the form of `{git_sha}` or `{git_tag}`,
        and the ecosystem disambiguates.
        "#,
    );
    quote! {
        #doc
    }
}
