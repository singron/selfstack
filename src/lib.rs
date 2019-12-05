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
    let mut init_fields = syn::punctuated::Punctuated::<syn::FieldValue, syn::Token![,]>::new();
    match &struct_def.fields {
        syn::Fields::Named(ref fns) => {
            for field in &fns.named {
                let fname = &field.ident;
                init_fields.push(syn::parse_quote!(
                        #fname: ::std::mem::MaybeUninit::uninit()
                ));
            }
        }
        _ => panic!("struct must have named fields"),
    }

    let impl_res = syn::parse_quote! {
        impl<'a> #sname {
            #[inline]
            #vis fn new() -> Self {
                 #sname { #init_fields }
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
    let mut view_fields: syn::FieldsNamed = syn::parse_quote!({
        _use_lt_a: ::std::marker::PhantomData<&'b &'a ()>,
    });
    let mut view_field_refs = syn::punctuated::Punctuated::<syn::FieldValue, syn::Token![,]>::new();
    view_field_refs.push(syn::parse_quote!(_use_lt_a: ::std::marker::PhantomData));
    match &mut struct_def.fields {
        syn::Fields::Named(ref mut fns) => {
            for field in fns.named.iter_mut() {
                let (build_name, try_build_name, substruct_name, viewstruct_name, mut_name) =
                    match &field.ident {
                        None => panic!("only named fields"),
                        Some(ident) => {
                            fields.insert(ident.clone(), true);
                            (
                                format!("build_{}", ident),
                                format!("try_build_{}", ident),
                                format!("{}_{}", struct_def.ident, ident),
                                format!("{}_View_{}", struct_def.ident, ident),
                                format!("mut_{}", ident),
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
                        *p = syn::parse_quote!(::std::mem::MaybeUninit<#p>);
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
                let mut_ident = syn::Ident::new(&mut_name, proc_macro2::Span::call_site());
                let build_ident = syn::Ident::new(&build_name, proc_macro2::Span::call_site());
                let try_build_ident =
                    syn::Ident::new(&try_build_name, proc_macro2::Span::call_site());
                let substruct_ident =
                    syn::Ident::new(&substruct_name, proc_macro2::Span::call_site());
                let viewstruct_ident =
                    syn::Ident::new(&viewstruct_name, proc_macro2::Span::call_site());
                let borrow = impls.len() == 1;
                let build: syn::ImplItem = if borrow {
                    syn::parse_quote! {
                        #vis fn #build_ident(&'a mut self, #field_ident: #ty_lt_a) -> #substruct_ident<'a> {
                            let #field_ident = unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)};
                            unsafe{::std::ptr::write(self.#field_ident.as_mut_ptr(), #field_ident)};
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
                            let store = self.store as *mut #sname;
                            ::std::mem::forget(self);
                            let store = unsafe{&mut*store};
                            let #field_ident = {
                                let #field_ident = initf(#store_refs);
                                unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)}
                            };
                            unsafe{::std::ptr::write(store.#field_ident.as_mut_ptr(), #field_ident)};
                            #substruct_ident{
                                store: store,
                            }
                        }
                    }
                };
                impls.last_mut().unwrap().items.push(build);
                if !borrow {
                    let trybuild = syn::parse_quote! {
                        #vis fn #try_build_ident<F, E>(mut self, initf: F) -> Result<#substruct_ident<'a>, E>
                            where F: FnOnce(#field_refs) -> Result<#ty_lt_b, E>
                        {
                            let store = self.store as *mut #sname;
                            ::std::mem::forget(self);
                            let store = unsafe{&mut*store};
                            let #field_ident = {
                                let #field_ident = initf(#store_refs)?;
                                unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)}
                            };
                            unsafe{::std::ptr::write(store.#field_ident.as_mut_ptr(), #field_ident)};
                            Ok(#substruct_ident{
                                store: store,
                            })
                        }
                    };
                    impls.last_mut().unwrap().items.push(trybuild);
                }
                let substruct_def = syn::parse_quote! {
                    #vis struct #substruct_ident<'a> {
                        store: &'a mut #sname,
                    }
                };
                structs.push(substruct_def);
                drop_stmts.stmts.insert(
                    0,
                    syn::parse_quote! {
                        unsafe{::std::ptr::drop_in_place(self.store.#field_ident.as_mut_ptr())};
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
                        unsafe{::std::mem::transmute::<&'_ #ty_lt_a, &'a #ty_lt_a>(&*store.#field_ident.as_ptr())}));
                field_getters.push(syn::parse_quote! {
                    #vis fn #field_ident(&'a self) -> &#ty_lt_a {
                        unsafe{::std::mem::transmute::<&'_ #ty_lt_static, &'a #ty_lt_a>(&*self.store.#field_ident.as_ptr())}
                    }
                });
                for getter in &field_getters {
                    impls.last_mut().unwrap().items.push(getter.clone());
                }
                // The mut getter is a little more complicated. Interior
                // lifetimes in mut references aren't covariant, unlike const
                // references. In this case, the interior 'static lifetime
                // isn't automatically subtyped to 'a. The danger is that if T
                // contains a mutable reference, we could set that reference
                // to something that lives as long as 'a, but supertype thinks
                // it needs to live as long as 'static. Normally that would be
                // correct, but in this case, the 'static lifetime is a lie to
                // make the store struct compilable, and the field will be
                // dropped after 'a.
                let mut_getter = syn::parse_quote! {
                    #vis fn #mut_ident(&'b mut self) -> &'b mut #ty_lt_a {
                        unsafe{::std::mem::transmute::<&'b mut #ty_lt_static, &'b mut #ty_lt_a>(
                                &mut *self.store.#field_ident.as_mut_ptr())}
                    }
                };
                impls.last_mut().unwrap().items.push(mut_getter);
                if !view_field_refs.empty_or_trailing() {
                    view_field_refs.push_punct(syn::Token![,](proc_macro2::Span::call_site()));
                }
                view_field_refs.push(syn::parse_quote!(
                        #field_ident: unsafe{::std::mem::transmute::<
                            &'b mut #ty_lt_static, &'b mut #ty_lt_a>(
                                &mut *self.store.#field_ident.as_mut_ptr())}));
                let view_struct_expr: syn::Expr = syn::parse_quote! {
                        #viewstruct_ident{
                            #view_field_refs
                        }
                };
                let view_getter = syn::parse_quote! {
                    #vis fn view(&'b mut self) -> #viewstruct_ident<'a, 'b> {
                        return #view_struct_expr;
                    }
                };
                impls.last_mut().unwrap().items.push(view_getter);
                view_field_refs.pop();
                view_field_refs.push(
                    syn::parse_quote!(#field_ident: unsafe{&*self.store.#field_ident.as_ptr()}),
                );
                let mut_view_field: syn::ItemStruct =
                    syn::parse_quote!(struct dummy {#vis #field_ident: &'b mut #ty_lt_a});
                view_fields
                    .named
                    .push(mut_view_field.fields.iter().next().unwrap().clone());
                structs.push(syn::parse_quote! {
                    #vis struct #viewstruct_ident<'a: 'b, 'b>
                        #view_fields
                });
                view_fields.named.pop();
                let const_view_field: syn::ItemStruct =
                    syn::parse_quote!(struct dummy {#vis #field_ident: &'b #ty_lt_a});
                view_fields
                    .named
                    .push(const_view_field.fields.iter().next().unwrap().clone());
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
