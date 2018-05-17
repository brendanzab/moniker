extern crate proc_macro;

extern crate quote;
extern crate syn;
#[macro_use]
extern crate synstructure;

use synstructure::{BindStyle, Structure};

decl_derive!([BoundTerm] => term_derive);

fn term_derive(mut s: Structure) -> quote::Tokens {
    s.bind_with(|_| BindStyle::Ref);

    let term_eq_body = {
        let body = s.variants().iter().fold(quote!(), |acc, v| {
            // Create two sets of bindings, one for the lhs, and another for the rhs
            let mut lhs = v.clone();
            let mut rhs = v.clone();
            lhs.binding_name(|_, i| syn::Ident::from(format!("__binding_lhs_{}", i)));
            rhs.binding_name(|_, i| syn::Ident::from(format!("__binding_rhs_{}", i)));

            let lhs_pat = lhs.pat();
            let rhs_pat = rhs.pat();

            // build up the alpha-equality expression for this variant
            let arm_body = <_>::zip(lhs.bindings().iter(), rhs.bindings()).fold(
                quote!(true),
                |acc, (lhs, rhs)| {
                    quote! { #acc && ::nameless::BoundTerm::term_eq(#lhs, #rhs) }
                },
            );

            quote! { #acc (&#lhs_pat, &#rhs_pat) => #arm_body, }
        });

        // Avoid the 'unreachable match' warning for types with zero or one variants
        match s.variants().len() {
            0 | 1 => body,
            _ => quote! { #body (_, _) => false },
        }
    };

    s.bind_with(|_| BindStyle::RefMut);

    let close_term_body = s.each(|bi| {
        quote!{
            ::nameless::BoundTerm::close_term(#bi, __state, __pattern);
        }
    });

    let open_term_body = s.each(|bi| {
        quote!{
            ::nameless::BoundTerm::open_term(#bi, __state, __pattern);
        }
    });

    s.bound_impl(
        quote!(::nameless::BoundTerm),
        quote! {
            fn term_eq(&self, other: &Self) -> bool {
                match (self, other) { #term_eq_body }
            }

            fn close_term(
                &mut self,
                __state: ::nameless::ScopeState,
                __pattern: &impl ::nameless::BoundPattern,
            ) {
                match *self { #close_term_body }
            }

            fn open_term(
                &mut self,
                __state: ::nameless::ScopeState,
                __pattern: &impl ::nameless::BoundPattern,
            ) {
                match *self { #open_term_body }
            }
        },
    )
}
