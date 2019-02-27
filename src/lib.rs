// Our macros are just complicated enough to blow past default limits.
#![recursion_limit = "128"]
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use std::collections::HashMap;
use syn::spanned::Spanned;

fn replace_lifetimes(ty: &mut syn::Type, lt: syn::Lifetime) -> Result<(), syn::Error> {
    match ty {
        syn::Type::Path(ref mut p) => {
            for seg in &mut p.path.segments {
                match &mut seg.arguments {
                    syn::PathArguments::AngleBracketed(ref mut args) => {
                        for mut arg in &mut args.args {
                            match &mut arg {
                                syn::GenericArgument::Lifetime(l) => {
                                    *l = lt.clone();
                                }
                                syn::GenericArgument::Type(ref mut t) => {
                                    replace_lifetimes(t, lt.clone())?;
                                }
                                _ => (),
                            }
                        }
                    }
                    _ => (),
                }
            }
        }
        ref x => {
            let msg = format!("type not supported {:?}", &x);
            return Err(syn::Error::new(ty.span(), msg));
        }
    };
    Ok(())
}

#[proc_macro_attribute]
pub fn selfstack(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut struct_def = syn::parse_macro_input!(item as syn::ItemStruct);
    let mut fields: HashMap<syn::Ident, bool> = HashMap::new();
    let sname = &struct_def.ident;
    let mut new_params = syn::punctuated::Punctuated::new();
    let vis = &struct_def.vis;
    for p in &struct_def.generics.params {
        match p {
            syn::GenericParam::Lifetime(_) => (),
            _ => new_params.push(p.clone()),
        }
    }
    struct_def.generics.params = new_params;
    let impl_res = syn::parse_quote! {
        impl<'a> #sname {
            #[inline]
             #vis fn new() -> Self {
                unsafe{::std::mem::uninitialized()}
            }
        }
    };
    let mut impls: Vec<syn::ItemImpl> = vec![impl_res];
    let mut structs: Vec<syn::ItemStruct> = vec![];
    let mut field_refs: syn::punctuated::Punctuated<syn::Type, syn::Token![,]> =
        syn::punctuated::Punctuated::new();
    let mut store_refs: syn::punctuated::Punctuated<syn::Expr, syn::Token![,]> =
        syn::punctuated::Punctuated::new();
    let mut drop_stmts: syn::Block = syn::parse_quote!({});
    let mut field_getters: Vec<syn::ImplItem> = vec![];
    match &mut struct_def.fields {
        syn::Fields::Named(ref mut fns) => {
            for field in fns.named.iter_mut() {
                let (build_name, substruct_name) = match &field.ident {
                    None => panic!("only named fields"),
                    Some(ident) => {
                        fields.insert(ident.clone(), true);
                        (
                            format!("build_{}", ident),
                            format!("{}_{}", struct_def.ident, ident),
                        )
                    }
                };
                match &field.vis {
                    syn::Visibility::Inherited => (),
                    x => {
                        return syn::Error::new(x.span(), "fields of a selfstack must be private")
                            .to_compile_error()
                            .into();
                    }
                }
                let orig_ty = field.ty.clone();
                if let Err(e) = replace_lifetimes(&mut field.ty, syn::parse_quote!('static)) {
                    return e.to_compile_error().into();
                }
                let ty_lt_static = field.ty.clone();
                match &mut field.ty {
                    syn::Type::Path(ref mut p) => {
                        *p = syn::parse_quote!(::std::mem::ManuallyDrop<#p>);
                    }
                    _ => panic!(),
                };
                let mut ty_lt_a = orig_ty.clone();
                if let Err(e) = replace_lifetimes(&mut ty_lt_a, syn::parse_quote!('a)) {
                    return e.to_compile_error().into();
                }
                let mut ty_lt_b = orig_ty.clone();
                if let Err(e) = replace_lifetimes(&mut ty_lt_b, syn::parse_quote!('b)) {
                    return e.to_compile_error().into();
                }
                let mut ty_lt__ = orig_ty.clone();
                if let Err(e) = replace_lifetimes(&mut ty_lt__, syn::parse_quote!('_)) {
                    return e.to_compile_error().into();
                }
                let field_ident = &field.ident;
                let build_ident = syn::Ident::new(&build_name, proc_macro2::Span::call_site());
                let substruct_ident =
                    syn::Ident::new(&substruct_name, proc_macro2::Span::call_site());
                let borrow = impls.len() == 1;
                let build: syn::ImplItem = if borrow {
                    syn::parse_quote! {
                        #vis fn #build_ident(&'a mut self, #field_ident: #ty_lt_a) -> #substruct_ident<'a> {
                            let #field_ident = unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)};
                            self.#field_ident = ::std::mem::ManuallyDrop::new(#field_ident);
                            #substruct_ident{
                                store: self,
                            }
                        }
                    }
                } else {
                    syn::parse_quote! {
                        #vis fn #build_ident<F>(mut self, initf: F) -> #substruct_ident<'a>
                            where F: FnOnce(#field_refs) -> #ty_lt_b
                        {
                            let store = unsafe{::std::mem::replace(&mut self.store, ::std::mem::uninitialized())};
                            ::std::mem::forget(self);
                            let #field_ident = {
                                let #field_ident = initf(#store_refs);
                                unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)}
                            };
                            store.#field_ident = ::std::mem::ManuallyDrop::new(#field_ident);
                            #substruct_ident{
                                store: &mut*store,
                            }
                        }
                    }
                };
                impls.last_mut().unwrap().items.push(build);
                let substruct_def = syn::parse_quote! {
                    #vis struct #substruct_ident<'a> {
                        store: &'a mut #sname,
                    }
                };
                structs.push(substruct_def);
                drop_stmts.stmts.insert(
                    0,
                    syn::parse_quote! {
                        unsafe{::std::mem::ManuallyDrop::drop(&mut self.store.#field_ident)};
                    },
                );
                let dropimpl = syn::parse_quote! {
                    impl<'a> Drop for #substruct_ident<'a> {
                        fn drop(&mut self) {
                            #drop_stmts
                        }
                    }
                };
                impls.push(dropimpl);
                let subimpl = syn::parse_quote! {
                    impl<'a: 'b, 'b> #substruct_ident<'a> {
                    }
                };
                impls.push(subimpl);
                field_refs.push(syn::parse_quote!(&'a #ty_lt_a));
                store_refs.push(syn::parse_quote!(
                        unsafe{::std::mem::transmute::<&'_ #ty_lt_a, &'a #ty_lt_a>(&store.#field_ident)}));
                field_getters.push(syn::parse_quote! {
                    #vis fn #field_ident(&'a self) -> &#ty_lt_a {
                        &self.store.#field_ident
                    }
                });
                for getter in &field_getters {
                    impls.last_mut().unwrap().items.push(getter.clone());
                }
            }
        }
        _ => panic!("only named fields supported"),
    };
    let res = quote!(#struct_def);
    let mut out = res.into();
    for s in structs {
        s.to_tokens(&mut out);
    }
    for i in impls {
        i.to_tokens(&mut out);
    }
    out.into()
}
