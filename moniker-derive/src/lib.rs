#![recursion_limit = "128"]

extern crate quote;
extern crate syn;
#[macro_use]
extern crate synstructure;
extern crate proc_macro2;

use syn::{Attribute, Lit, Meta, NestedMeta};
use synstructure::{BindStyle, Structure};

decl_derive!([BoundTerm] => term_derive);

fn term_derive(mut s: Structure) -> proc_macro2::TokenStream {
    let opts = TopLevelOptions::from_attrs(&s.ast().attrs);

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
                    quote! { #acc && moniker::BoundTerm::<String>::term_eq(#lhs, #rhs) }
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
        quote!{ moniker::BoundTerm::<String>::close_term(#bi, __state, __pattern); }
    });
    let open_term_body = s.each(|bi| {
        quote!{ moniker::BoundTerm::<String>::open_term(#bi, __state, __pattern); }
    });

    s.bind_with(|_| BindStyle::Ref);
    let visit_vars_body = s.each(|bi| {
        quote!{ moniker::BoundTerm::<String>::visit_vars(#bi, __on_var); }
    });

    s.bind_with(|_| BindStyle::RefMut);
    let visit_mut_vars_body = s.each(|bi| {
        quote!{ moniker::BoundTerm::<String>::visit_mut_vars(#bi, __on_var); }
    });

    let (ident, bounds) = match opts.ident_ty {
        Some(ref ident) => (quote! { #ident }, quote!{}),
        None => (
            quote! { __Ident },
            quote! { where __Ident: PartialEq + Clone },
        ),
    };

    s.gen_impl(quote! {
        extern crate moniker;

        gen impl moniker::BoundTerm<#ident> for @Self #bounds {
            fn term_eq(&self, other: &Self) -> bool {
                match (self, other) { #term_eq_body }
            }

            fn close_term(
                &mut self,
                __state: moniker::ScopeState,
                __pattern: &impl moniker::BoundPattern<#ident>,
            ) {
                match *self { #close_term_body }
            }

            fn open_term(
                &mut self,
                __state: moniker::ScopeState,
                __pattern: &impl moniker::BoundPattern<#ident>,
            ) {
                match *self { #open_term_body }
            }

            fn visit_vars(&self, __on_var: &mut impl FnMut(&moniker::Var<#ident>)) {
                match *self { #visit_vars_body }
            }

            fn visit_mut_vars(&mut self, __on_var: &mut impl FnMut(&mut moniker::Var<#ident>)) {
                match *self { #visit_mut_vars_body }
            }
        }
    })
}

fn moniker_metas<'a>(attrs: &'a [Attribute]) -> impl Iterator<Item = Meta> + 'a {
    attrs
        .iter()
        .filter_map(Attribute::interpret_meta)
        .filter(|meta| meta.name() == "moniker")
        .flat_map(|meta| match meta {
            Meta::Word(_) => panic!("invalid annotation"),
            Meta::NameValue(_) => panic!("invalid annotation"),
            Meta::List(list) => list.nested.into_iter().map(|meta| match meta {
                NestedMeta::Meta(meta) => meta,
                NestedMeta::Literal(_) => panic!("invalid annotation"),
            }),
        })
}

struct TopLevelOptions {
    ident_ty: Option<String>,
}

impl TopLevelOptions {
    fn from_attrs(attrs: &[Attribute]) -> TopLevelOptions {
        let mut opts = TopLevelOptions { ident_ty: None };

        for meta in moniker_metas(attrs) {
            if meta.name() == "ident" {
                if opts.ident_ty.is_some() {
                    panic!("`#[moniker(ident = ...)]` was already set!");
                }

                match meta {
                    Meta::Word(_) | Meta::List(_) => {
                        panic!("expected binding in `#[moniker(ident = ...)]`")
                    },
                    Meta::NameValue(kv) => {
                        if let Lit::Str(ref s) = kv.lit {
                            opts.ident_ty = Some(s.value());
                        } else {
                            panic!("expected string in `#[moniker(ident = ...)]`");
                        }
                    },
                }
            } else {
                panic!("unexpected attribute");
            }
        }

        opts
    }
}
