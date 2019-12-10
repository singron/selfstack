// Our macros are just complicated enough to blow past default limits.
#![recursion_limit = "128"]
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
use proc_macro::TokenStream;
// quote is used by syn::parse_quote
#[allow(unused_imports)]
use quote::quote;
use quote::ToTokens;
use syn::spanned::Spanned;

/// Replace all lifetimes in ty with the lifetime lt.
fn replace_lifetimes(ty: &mut syn::Type, lt: syn::Lifetime) -> Result<(), syn::Error> {
    match ty {
        syn::Type::Path(ref mut p) => {
            for seg in &mut p.path.segments {
                match &mut seg.arguments {
                    syn::PathArguments::Parenthesized(ref mut args) => {
                        for mut input in &mut args.inputs {
                            replace_lifetimes(&mut input, lt.clone())?;
                        }
                        if let syn::ReturnType::Type(_, ty) = &mut args.output {
                            replace_lifetimes(ty, lt.clone())?;
                        }
                    }
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
                    syn::PathArguments::None => (),
                }
            }
        }
        syn::Type::Reference(ref mut r) => {
            if let Some(ref mut l) = &mut r.lifetime {
                *l = lt;
            }
        }
        ref x => {
            // TODO: Handle more types.
            let msg = format!("type not supported: {:?}", &x);
            return Err(syn::Error::new(ty.span(), msg));
        }
    };
    Ok(())
}

// proc_macros cannot be statements, but rustc thinks item proc_macros are statements in doctests.
// If we use main(), then it forces it to be an item.
#[allow(clippy::needless_doctest_main)]
#[proc_macro]
/// selfstack produces a stack-like self-referential data structure with a safe interface. This is
/// safe because layers in the stack can only reference layers below them, and lower layers outlive
/// higher layers. This restriction prevents cycles, dangling references, and other unsoundness
/// that would generally be possible with self-reference.
///
/// You must declare a mod in this macro. This is so the macro can make unsafe operations private.
///
/// Any struct inside that mod defines the layers of a stack with its fields. This struct is the
/// Store. It provides storage for all fields ahead of time, but at first is uninitialized.
/// Lifetime names are ignored, but you may use them to document references.
///
/// ```
/// selfstack::selfstack! {
///     mod mystack {
///         pub struct MyStore {
///             layer1: u32,
///             layer2: &'layer1 u32,
///         }
///     }
/// }
/// # fn main() {}
/// ```
///
/// To initialize the first layer, call `set_$field(T)` on the Store with the value of the first
/// field. This will return a SubStruct that will allow you to safely access the subset of
/// initialized layers.
///
/// ```
/// # selfstack::selfstack! {
/// #     mod mystack {
/// #         pub struct MyStore {
/// #             layer1: u32,
/// #             layer2: &'layer1 u32,
/// #         }
/// #     }
/// # }
/// #
/// # fn main() {
/// let mut store = mystack::MyStore::new();
/// let sub_struct = store.set_layer1(42);
/// # }
/// ```
///
///
/// You can initialize further layers with the `build_$field()` and `try_build_$field()` methods.
/// These return SubStructs that will allow access to the next layer. For `build()`, you pass a
/// closure that takes references to the previous fields and returns the value of the next field.
/// `try_build()` is the same except the closure should return a `Result` and it will return a
/// `Result`.
///
/// ```
/// # selfstack::selfstack! {
/// #     mod mystack {
/// #         pub struct MyStore {
/// #             layer1: u32,
/// #             layer2: &'layer1 u32,
/// #         }
/// #     }
/// # }
/// #
/// # fn main() {
/// # let mut store = mystack::MyStore::new();
/// # let sub_struct = store.set_layer1(42);
/// let sub_struct = sub_struct.build_layer2(|layer1: &u32|->&u32 {
///     layer1
/// });
/// # }
/// ```
///
/// You can get a const reference to any layer or a mutable reference to the top-most layer using
/// methods that match ref_$fieldname() or mut_$fieldname(). If you need references to multiple
/// layers simultaneously, you can call the view() method which will return a ViewStruct that
/// contains public fields referencing each layer [^0].
///
/// ```
/// # selfstack::selfstack! {
/// #     mod mystack {
/// #         pub struct MyStore {
/// #             layer1: u32,
/// #             layer2: &'layer1 u32,
/// #         }
/// #     }
/// # }
/// #
/// # fn main() {
/// # let mut store = mystack::MyStore::new();
/// # let sub_struct = store.set_layer1(42);
/// # let mut sub_struct = sub_struct.build_layer2(|layer1: &u32|->&u32 {
/// #     &layer1
/// # });
/// assert_eq!(*sub_struct.ref_layer1(), 42);
/// let view = sub_struct.view();
/// assert_eq!(**view.layer2, 42);
/// assert_eq!(*view.layer2, view.layer1);
/// *view.layer2 = &0; // Top layer is mutable.
/// assert_eq!(**view.layer2, 0);
/// # }
/// ```
///
/// When the SubStruct is dropped, the initialized fields will be dropped in reverse order and the
/// Store can be reused.
///
/// [^0]: This cludge is due to limitations in the borrow checker. Calling a method on the
/// SubStruct borrows the entire SubStruct, and the borrow checker won't allow multiple borrows
/// simultaneously if any are mutable. The borrow checker is able to allow simultaneous borrows to
/// the individual fields of a struct however.
pub fn selfstack(item: TokenStream) -> TokenStream {
    let mut mod_def = syn::parse_macro_input!(item as syn::ItemMod);
    if let Some((_, content)) = &mut mod_def.content {
        let input_content = std::mem::replace(content, Vec::new());
        for item in input_content {
            match item {
                syn::Item::Struct(s) => {
                    if let Err(e) = selfstack_struct(s, content) {
                        return e.to_compile_error().into();
                    }
                }
                syn::Item::Use(u) => {
                    content.push(syn::Item::Use(u));
                }
                _ => {
                    // Allowing other items inside the mod (like fn and impl) could circumvent the
                    // safe interface.
                    return syn::Error::new_spanned(item, "item not supported in a selfstack mod")
                        .to_compile_error()
                        .into();
                }
            }
        }
    }
    let mut out = proc_macro2::TokenStream::new();
    mod_def.to_tokens(&mut out);
    out.into()
}

/// This is invoked for each input struct and should push all output Items to out.
fn selfstack_struct(
    mut struct_def: syn::ItemStruct,
    out: &mut Vec<syn::Item>,
) -> Result<(), syn::Error> {
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
    let call_site = proc_macro2::Span::call_site();
    // The _Ptrs struct will hold raw pointers to each field created up front. If we were to create
    // references from the store directly as needed, that would invalidate existing references
    // according to stacked borrows.
    let store_ptrs_ident = syn::Ident::new(&format!("{}_Ptrs", sname), call_site);
    let mut init_field_values =
        syn::punctuated::Punctuated::<syn::FieldValue, syn::Token![,]>::new();
    let mut raw_ptr_field_values =
        syn::punctuated::Punctuated::<syn::FieldValue, syn::Token![,]>::new();
    let mut raw_ptr_fields: syn::FieldsNamed = syn::parse_quote!({});
    let mut impls: Vec<syn::ItemImpl> = vec![];
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

    let store_impl = syn::parse_quote! {
        impl<'a> #sname {
        }
    };
    impls.push(store_impl);

    let struct_fields = match &mut struct_def.fields {
        syn::Fields::Named(ref mut fns) => fns,
        // We could possibly use numbered fields, but that currently seems too complicated for the
        // benefit.
        _ => {
            return Err(syn::Error::new(
                struct_def.span(),
                "struct must have named fields",
            ));
        }
    };
    for field in struct_fields.named.iter_mut() {
        let field_ident = field.ident.as_ref().unwrap();
        let build_ident = syn::Ident::new(&format!("build_{}", field_ident), call_site);
        let set_ident = syn::Ident::new(&format!("set_{}", field_ident), call_site);
        let try_build_ident = syn::Ident::new(&format!("try_build_{}", field_ident), call_site);
        let substruct_ident =
            syn::Ident::new(&format!("{}_{}", struct_def.ident, field_ident), call_site);
        let viewstruct_ident = syn::Ident::new(
            &format!("{}_View_{}", struct_def.ident, field_ident),
            call_site,
        );
        let mut_ident = syn::Ident::new(&format!("mut_{}", field_ident), call_site);
        let ref_ident = syn::Ident::new(&format!("ref_{}", field_ident), call_site);

        match &field.vis {
            syn::Visibility::Inherited => (),
            x => {
                return Err(syn::Error::new(
                    x.span(),
                    "fields of a selfstack must be private",
                ))
            }
        }
        let orig_ty = field.ty.clone();
        // We allow bogus lifetimes in order to express self-reference, so we have to erase the
        // lifetimes in the struct definition by using 'static.
        replace_lifetimes(&mut field.ty, syn::parse_quote!('static))?;
        let ty_lt_static = field.ty.clone();
        {
            let fty = &field.ty;
            field.ty = syn::parse_quote!(::std::mem::MaybeUninit<#fty>);
        }
        // Prepare some types with single lifetimes so that generated functions can just use 'a,
        // 'b, and '_.
        let mut ty_lt_a = orig_ty.clone();
        replace_lifetimes(&mut ty_lt_a, syn::parse_quote!('a))?;
        let mut ty_lt_b = orig_ty.clone();
        replace_lifetimes(&mut ty_lt_b, syn::parse_quote!('b))?;
        let mut ty_lt__ = orig_ty.clone();
        replace_lifetimes(&mut ty_lt__, syn::parse_quote!('_))?;
        // The first layer has no previous layers to reference, so use set instead of
        // build.
        let is_set = impls.len() == 1;
        let build: syn::ImplItem = if is_set {
            syn::parse_quote! {
                #[inline]
                #vis fn #set_ident(&'a mut self, #field_ident: #ty_lt_a) -> #substruct_ident<'a> {
                    let ptrs = self.ptrs();
                    let #field_ident = unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)};
                    unsafe{::std::ptr::write(ptrs.#field_ident, #field_ident)};
                    #substruct_ident{
                        _store: ::std::marker::PhantomData,
                        ptrs,
                    }
                }
            }
        } else {
            syn::parse_quote! {
                #[inline]
                #vis fn #build_ident<F>(mut self, initf: F) -> #substruct_ident<'a>
                    where F: FnOnce(#field_refs) -> #ty_lt_b
                {
                    let ptrs = self.ptrs;
                    ::std::mem::forget(self);
                    let #field_ident = {
                        let #field_ident = initf(#store_refs);
                        unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)}
                    };
                    unsafe{::std::ptr::write(ptrs.#field_ident, #field_ident)};
                    #substruct_ident{
                        _store: ::std::marker::PhantomData,
                        ptrs,
                    }
                }
            }
        };
        impls.last_mut().unwrap().items.push(build);
        if !is_set {
            let trybuild = syn::parse_quote! {
                #[inline]
                #vis fn #try_build_ident<F, E>(mut self, initf: F) -> Result<#substruct_ident<'a>, E>
                    where F: FnOnce(#field_refs) -> Result<#ty_lt_b, E>
                {
                    let ptrs = self.ptrs;
                    ::std::mem::forget(self);
                    let #field_ident = {
                        let #field_ident = initf(#store_refs)?;
                        unsafe{::std::mem::transmute::<#ty_lt__, #ty_lt_static>(#field_ident)}
                    };
                    unsafe{::std::ptr::write(ptrs.#field_ident, #field_ident)};
                    Ok(#substruct_ident{
                        _store: ::std::marker::PhantomData,
                        ptrs,
                    })
                }
            };
            impls.last_mut().unwrap().items.push(trybuild);
        }
        let substruct_def = syn::parse_quote! {
            #vis struct #substruct_ident<'a> {
                _store: ::std::marker::PhantomData<&'a mut #sname>,
                ptrs: #store_ptrs_ident,
            }
        };
        structs.push(substruct_def);
        drop_stmts.stmts.insert(
            0,
            syn::parse_quote! {
                unsafe{::std::ptr::drop_in_place(self.ptrs.#field_ident)};
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
                        unsafe{::std::mem::transmute::<&'_ #ty_lt_a, &'a #ty_lt_a>(&*(ptrs.#field_ident as *const _))}));
        field_getters.push(syn::parse_quote! {
                    #[inline]
                    #vis fn #ref_ident(&'a self) -> &#ty_lt_a {
                        unsafe{::std::mem::transmute::<&'_ #ty_lt_static, &'a #ty_lt_a>(&*(self.ptrs.#field_ident as *const _))}
                    }
                });
        for getter in &field_getters {
            impls.last_mut().unwrap().items.push(getter.clone());
        }
        // The mut getter is a little more complicated. Interior lifetimes in mut references aren't
        // covariant, unlike const references. In this case, the interior 'static lifetime isn't
        // automatically subtyped to 'a. The danger is that if T contains a mutable reference, we
        // could set that reference to something that lives as long as 'a, but supertype thinks it
        // needs to live as long as 'static. Normally that would be correct, but in this case, the
        // 'static lifetime is a lie to make the store struct compilable, and the field will be
        // dropped after 'a.
        let mut_getter = syn::parse_quote! {
            #[inline]
            #vis fn #mut_ident(&'b mut self) -> &'b mut #ty_lt_a {
                unsafe{::std::mem::transmute::<&'b mut #ty_lt_static, &'b mut #ty_lt_a>(
                        &mut *self.ptrs.#field_ident)}
            }
        };
        impls.last_mut().unwrap().items.push(mut_getter);
        if !view_field_refs.empty_or_trailing() {
            view_field_refs.push_punct(syn::Token![,](call_site));
        }
        view_field_refs.push(syn::parse_quote!(
                        #field_ident: unsafe{::std::mem::transmute::<
                            &'b mut #ty_lt_static, &'b mut #ty_lt_a>(
                                &mut *self.ptrs.#field_ident)}));
        let view_struct_expr: syn::Expr = syn::parse_quote! {
                #viewstruct_ident{
                    #view_field_refs
                }
        };
        let view_getter = syn::parse_quote! {
            #[inline]
            #vis fn view(&'b mut self) -> #viewstruct_ident<'a, 'b> {
                return #view_struct_expr;
            }
        };
        impls.last_mut().unwrap().items.push(view_getter);
        view_field_refs.pop();
        view_field_refs
            .push(syn::parse_quote!(#field_ident: unsafe{&*(self.ptrs.#field_ident as *const _)}));
        // syn::parse_quote! doesn't handle struct field definitions. Instead, we use a dummy
        // struct and extract the first field definition from the parsed value.
        fn first_field(s: syn::ItemStruct) -> syn::Field {
            s.fields.iter().next().unwrap().clone()
        }
        macro_rules! parse_field {
            ($($t:tt)*) => {
                first_field(syn::parse_quote!(struct dummy { $($t)* }))
            };
        };
        let mut_view_field = parse_field!(#vis #field_ident: &'b mut #ty_lt_a);
        view_fields.named.push(mut_view_field);
        structs.push(syn::parse_quote! {
            #vis struct #viewstruct_ident<'a: 'b, 'b>
                #view_fields
        });
        view_fields.named.pop();
        let const_view_field = parse_field!(#vis #field_ident: &'b #ty_lt_a);
        view_fields.named.push(const_view_field);
        init_field_values.push(syn::parse_quote!(
                #field_ident: ::std::mem::MaybeUninit::uninit()
        ));
        raw_ptr_field_values.push(syn::parse_quote!(
                #field_ident: self.#field_ident.as_mut_ptr()
        ));
        let raw_ptr_field = parse_field!(#field_ident: *mut #ty_lt_static);
        raw_ptr_fields.named.push(raw_ptr_field);
    }

    let store_impl = impls.first_mut().unwrap();
    store_impl.items.push(syn::parse_quote! {
        #[inline]
        #vis fn new() -> Self {
             #sname { #init_field_values }
        }
    });
    store_impl.items.push(syn::parse_quote! {
        #[inline]
        fn ptrs(&mut self) -> #store_ptrs_ident {
            #store_ptrs_ident {
                #raw_ptr_field_values
            }
        }
    });
    let store_ptrs_struct: syn::ItemStruct = syn::parse_quote! {
        #[derive(Copy,Clone)]
        struct #store_ptrs_ident
            #raw_ptr_fields
    };
    structs.push(store_ptrs_struct);

    out.push(syn::Item::Struct(struct_def));
    for s in structs {
        out.push(syn::Item::Struct(s));
    }
    for i in impls {
        out.push(syn::Item::Impl(i));
    }
    Ok(())
}
