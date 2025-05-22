//! Codegen macros for the `locator` crate.

use ecosystem::Invocation;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod ecosystem;

/// ...
#[proc_macro]
pub fn ecosystems(input: TokenStream) -> TokenStream {
    let invocation = parse_macro_input!(input as ecosystem::Invocation);

    let invalid_conversion_err = Invocation::mk_invalid_conversion_error();
    let ecosystems = invocation.mk_ecosystem_enum();
    let subsets = invocation.mk_ecosystem_category_enums();
    let structs = invocation.mk_ecosystem_structs();

    TokenStream::from(quote! {
        pub mod ecosystem {
            #invalid_conversion_err;
            #ecosystems
            #subsets
            #structs
        }
    })
}
