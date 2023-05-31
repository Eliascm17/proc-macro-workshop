extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

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
        let name = &f.ident;
        let ty = f.ty.clone();

        let set_method = if let Some(inner_ty) = ty_inner_type("Option", &ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else if builder_of(f).is_some() {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
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

    fn builder_of(f: &syn::Field) -> Option<proc_macro2::Group> {
        for attr in &f.attrs {
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
                if let TokenTree::Group(g) = attr.tts.clone().into_iter().next().unwrap() {
                    return Some(g);
                }
            }
        }
        None
    }

    fn extend_methods(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
        let name = f.ident.as_ref().unwrap();
        let g = builder_of(f)?;

        let mut tokens = g.stream().into_iter();

        match tokens.next().unwrap() {
            TokenTree::Ident(ref i) => assert_eq!(i, "each"),
            tt => panic!("expected 'each' found {}", tt),
        }

        match tokens.next().unwrap() {
            TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
            tt => panic!("expected '=' found {}", tt),
        }

        let arg = match tokens.next().unwrap() {
            TokenTree::Literal(l) => l,
            tt => panic!("expected string found {}", tt),
        };

        match syn::Lit::new(arg) {
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
            quote! { #name: Vec::new() }
        } else {
            quote! {
                #name: None
            }
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }
        impl #bident {
            #(#methods)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
        impl #name {
            fn builder() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };

    expanded.into()
}

// use proc_macro::TokenStream;
// use quote::quote;
// use syn::{parse_macro_input, DeriveInput};

// #[proc_macro_derive(Builder, attributes(builder))]
// pub fn derive(input: TokenStream) -> TokenStream {
//     // input
//     let ast = parse_macro_input!(input as DeriveInput);
//     eprintln!("{:#?}", ast);
//     // name of struct
//     let name = &ast.ident;
//     // struct name + "Builder"
//     let bname = format!("{}Builder", name);
//     // specify span for bname
//     let bident = syn::Ident::new(&bname, name.span());
//     // pulling fields out of struct
//     let fields = if let syn::Data::Struct(syn::DataStruct {
//         fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
//         ..
//     }) = ast.data
//     {
//         named
//     } else {
//         unimplemented!()
//     };

//     let builder_fields = fields.iter().map(|f| {
//         let name = &f.ident;
//         let ty = &f.ty;
//         if ty_inner_type("Option", ty).is_some() || builder_of(&f).is_some() {
//             quote! { #name: #ty }
//         } else {
//             quote! { #name: std::option::Option<#ty> }
//         }
//     });

//     let methods = fields.iter().map(|f| {
//         let name = f.ident.as_ref().unwrap();
//         let ty = &f.ty;

//         let (arg_type, value) = if let Some(inner_ty) = ty_inner_type("Option", ty) {
//             // if the field is an Option<T>, setting should take just a T,
//             // but we then need to store it within a Some.
//             (inner_ty, quote! { std::option::Option::Some(#name) })
//         } else if builder_of(&f).is_some() {
//             // if the field is a builder, it is a Vec<T>,
//             // and the value in the builder is _not_ wrapped in an Option,
//             // so we shouldn't wrap the value in Some.
//             (ty, quote! { #name })
//         } else {
//             // otherwise, we take the type used by the target,
//             // and we store it in an Option in the builder
//             // in case it was never set.
//             (ty, quote! { std::option::Option::Some(#name) })
//         };
//         let set_method = quote! {
//             pub fn #name(&mut self, #name: #arg_type) -> &mut Self {
//                 self.#name = #value;
//                 self
//             }
//         };

//         // we need to take care not to include a builder method with the same name as the set
//         // method. for example, consider this struct:
//         //
//         // ```
//         // #[derive(Builder)]
//         // struct Command {
//         //     #[builder(each = "env")]
//         //     env: Vec<String>
//         // }
//         // ```
//         //
//         // It would not be okay to generate both `env(Vec<String>)` for the field
//         // *and* `env(String)` for the builder.
//         match extend_method(&f) {
//             None => set_method.into(),
//             Some((true, extend_method)) => extend_method,
//             Some((false, extend_method)) => {
//                 // safe to generate both!
//                 let expr = quote! {
//                     #set_method
//                     #extend_method
//                 };
//                 expr.into()
//             }
//         }
//     });

//     let build_fields = fields.iter().map(|f| {
//         let name = &f.ident;
//         if ty_inner_type("Option", &f.ty).is_some() || builder_of(f).is_some() {
//             quote! {
//                 #name: self.#name.clone()
//             }
//         } else {
//             quote! {
//                 #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
//             }
//         }
//     });

//     let build_empty = fields.iter().map(|f| {
//         let name = &f.ident;
//         if builder_of(&f).is_some() {
//             quote! { #name: std::vec::Vec::new() }
//         } else {
//             quote! { #name: std::option::Option::None }
//         }
//     });

//     let doc = format!("\
//             Implements the [builder pattern] for [`{}`].\n\
//             \n\
//             [builder pattern]: https://rust-lang-nursery.github.io/api-guidelines/type-safety.html#c-builder", name);

//     quote! {
//         #[doc = #doc]
//         pub struct #bident {
//             #(#builder_fields,)*
//         }

//         impl #bident {
//             #(#methods)*

//             pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
//                 Ok(#name {
//                    #(#build_fields,)*
//                 })
//             }
//         }

//         impl #name {
//             pub fn builder() -> #bident {
//                 #bident {
//                     #(#build_empty,)*
//                 }
//             }
//         }
//     }
//     .into()
// }

// // returns Option<> of the attribute passed into: #[(builder = "env")] --> Some(env)
// fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
//     for attr in &f.attrs {
//         if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
//             return Some(attr);
//         }
//     }
//     None
// }

// fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
//     let name = f.ident.as_ref().unwrap();
//     let g = builder_of(f)?;

//     fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
//         Some((
//             false,
//             syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
//         ))
//     }

//     let meta = match g.parse_meta() {
//         Ok(syn::Meta::List(mut nvs)) => {
//             // list here is .. in #[builder(..)]
//             assert_eq!(nvs.ident, "builder");
//             if nvs.nested.len() != 1 {
//                 return mk_err(nvs);
//             }

//             // nvs.nested[0] here is (hopefully): each = "foo"
//             match nvs.nested.pop().unwrap().into_value() {
//                 syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
//                     if nv.ident != "each" {
//                         return mk_err(nvs);
//                     }
//                     nv
//                 }
//                 meta => {
//                     // nvs.nested[0] was not k = v
//                     return mk_err(meta);
//                 }
//             }
//         }
//         Ok(meta) => {
//             // inside of #[] there was either just an identifier (`#[builder]`) or a key-value
//             // mapping (`#[builder = "foo"]`), neither of which are okay.
//             return mk_err(meta);
//         }
//         Err(e) => {
//             return Some((false, e.to_compile_error()));
//         }
//     };
//     match meta.lit {
//         syn::Lit::Str(s) => {
//             let arg = syn::Ident::new(&s.value(), s.span());
//             let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
//             let method = quote! {
//                 pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
//                     self.#name.push(#arg);
//                     self
//                 }
//             };
//             Some((&arg == name, method))
//         }
//         lit => panic!("expected string, found {:?}", lit),
//     }
// }

// fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
//     if let syn::Type::Path(ref p) = ty {
//         if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
//             return None;
//         }

//         if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
//             if inner_ty.args.len() != 1 {
//                 return None;
//             }

//             let inner_ty = inner_ty.args.first().unwrap();
//             if let syn::GenericArgument::Type(ref t) = inner_ty.value() {
//                 return Some(t);
//             }
//         }
//     }
//     None
// }
