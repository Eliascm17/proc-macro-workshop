extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    eprintln!("{:#?}", ast);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };
    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = f.ty.clone();
        if ty_inner_type("Option", &ty).is_some() || builder_of(f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;

        let (arg_type, value) = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            // if the field is an Option<T>, setting should take just a T,
            // but we then need to store it with a Some.
            (inner_ty, quote! { std::option::Option::Some(#name) })
        } else if builder_of(f).is_some() {
            // if the field is a builder, it is a Vec<T>, and the value in the builder
            // is not wrapped in an Option, so we shouldn't wrap the value in Some
            (ty, quote! { #name })
        } else {
            // otherwise, we take the type used by the target,
            // and we store it in an Option in the builder
            // in case it was never set
            (ty, quote! { std::option::Option::Some(#name) })
        };

        let set_method = quote! {
            pub fn #name(&mut self, #name: #arg_type) -> &mut Self {
                self.#name = #value;
                self
            }
        };

        match extend_methods(f) {
            None => set_method,
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => {
                quote! {
                    #set_method
                    #extend_method
                }
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = f.ty.clone();
        if ty_inner_type("Option", &ty).is_some() || builder_of(f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! {
                #name: std::option::Option::None
            }
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }
        impl #bident {
            #(#methods)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };

    expanded.into()
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    return f
        .attrs
        .iter()
        .find(|&attr| attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder");
}

fn extend_methods(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;
    fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
        Some((
            false,
            syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
        ))
    }

    let meta = match g.parse_meta() {
        Ok(syn::Meta::List(mut nvs)) => {
            assert_eq!(nvs.ident, "builder");
            if nvs.nested.len() != 1 {
                return mk_err(nvs);
            }
            match nvs.nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    if nv.ident != "each" {
                        return mk_err(nvs);
                    }
                    nv
                }
                meta => return mk_err(meta),
            }
        }
        Ok(meta) => return mk_err(meta),
        Err(e) => return Some((false, e.to_compile_error())),
    };

    match meta.lit {
        syn::Lit::Str(s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
            let method = quote! { pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                    self.#name.push(#arg);
                    self
                }
            };
            Some((&arg == name, method))
        }
        lit => panic!("expected string, found {:?}", lit),
    }
}

// get inner type of type: Vec<String> -> String
fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty.value() {
                return Some(t);
            }
        }
    }
    None
}
