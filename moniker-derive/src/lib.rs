#![recursion_limit = "128"]

extern crate quote;
extern crate syn;
#[macro_use]
extern crate synstructure;
extern crate proc_macro2;

use synstructure::{BindStyle, Structure};

decl_derive!([BoundTerm] => term_derive);

fn term_derive(mut s: Structure) -> proc_macro2::TokenStream {
    s.bind_with(|_| BindStyle::Ref);
    let term_eq_body = {
        let body = s.variants().iter().fold(quote!(), |acc, v| {
            // Create two sets of bindings, one for the lhs, and another for the rhs
            let mut lhs = v.clone();
            let mut rhs = v.clone();
            lhs.binding_name(|_, i| {
                syn::Ident::new(
                    &format!("__binding_lhs_{}", i),
                    proc_macro2::Span::call_site(),
                )
            });
            rhs.binding_name(|_, i| {
                syn::Ident::new(
                    &format!("__binding_rhs_{}", i),
                    proc_macro2::Span::call_site(),
                )
            });

            let lhs_pat = lhs.pat();
            let rhs_pat = rhs.pat();

            // build up the alpha-equality expression for this variant
            let arm_body = <_>::zip(lhs.bindings().iter(), rhs.bindings()).fold(
                quote!(true),
                |acc, (lhs, rhs)| {
                    quote! { #acc && moniker::BoundTerm::term_eq(#lhs, #rhs) }
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
        quote!{ moniker::BoundTerm::close_term(#bi, __state, __pattern); }
    });
    let open_term_body = s.each(|bi| {
        quote!{ moniker::BoundTerm::open_term(#bi, __state, __pattern); }
    });

    s.bind_with(|_| BindStyle::Ref);
    let visit_vars_body = s.each(|bi| {
        quote!{ moniker::BoundTerm::visit_vars(#bi, __on_var); }
    });

    s.bind_with(|_| BindStyle::RefMut);
    let visit_mut_vars_body = s.each(|bi| {
        quote!{ moniker::BoundTerm::visit_mut_vars(#bi, __on_var); }
    });

    s.gen_impl(quote! {
        extern crate moniker;

        gen impl moniker::BoundTerm<String> for @Self {
            fn term_eq(&self, other: &Self) -> bool {
                match (self, other) { #term_eq_body }
            }

            fn close_term(
                &mut self,
                __state: moniker::ScopeState,
                __pattern: &impl moniker::BoundPattern<String>,
            ) {
                match *self { #close_term_body }
            }

            fn open_term(
                &mut self,
                __state: moniker::ScopeState,
                __pattern: &impl moniker::BoundPattern<String>,
            ) {
                match *self { #open_term_body }
            }

            fn visit_vars(&self, __on_var: &mut impl FnMut(&moniker::Var<String>)) {
                match *self { #visit_vars_body }
            }

            fn visit_mut_vars(&mut self, __on_var: &mut impl FnMut(&mut moniker::Var<String>)) {
                match *self { #visit_mut_vars_body }
            }
        }
    })
}
