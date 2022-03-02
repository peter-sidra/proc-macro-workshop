use proc_macro::TokenStream;
use proc_macro_error::{proc_macro_error, Diagnostic};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, DeriveInput,
    GenericArgument, Meta, MetaList, MetaNameValue, Path, PathArguments, PathSegment,
};

fn get_generic_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        if let Some(PathSegment {
            arguments:
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }),
            ..
        }) = segments.last()
        {
            if let GenericArgument::Type(ref generic_type) = args[0] {
                return Some(generic_type);
            }
        }
    }
    None
}

fn is_type_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        if let Some(PathSegment {
            ref ident,
            arguments:
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }),
        }) = segments.last()
        {
            if ident.to_string() == "Option" && args.len() == 1 {
                if let GenericArgument::Type(ref generic_type) = args[0] {
                    return Some(generic_type);
                }
            }
        }
    }
    None
}

fn try_parse_attr_name_value(
    attr_meta: &syn::Meta,
    name: impl Into<String>,
) -> Result<&syn::LitStr, proc_macro2::Span> {
    if let Meta::List(MetaList {
        path: Path { ref segments, .. },
        ref nested,
        ..
    }) = attr_meta
    {
        if segments.len() == 1 && segments[0].ident == "builder" {
            if let Some(syn::NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                path: syn::Path { ref segments, .. },
                lit: syn::Lit::Str(ref method_name),
                ..
            }))) = nested.first()
            {
                if segments.len() == 1 && segments[0].ident == name.into() {
                    return Ok(method_name);
                } else {
                    return Err(segments[0].ident.span());
                }
            }
        }
    }

    Err(proc_macro2::Span::call_site())
}

fn try_parse_attr_each(
    field: &syn::Field,
) -> Option<Result<syn::LitStr, proc_macro2::TokenStream>> {
    for attribute in &field.attrs {
        let attr_meta = attribute
            .parse_meta()
            .expect("Error while parsing an attribute");

        match try_parse_attr_name_value(&attr_meta, "each") {
            Ok(value) => {
                return Some(Ok(value.to_owned()));
            }
            Err(_err_span) => {
                // Diagnostic::spanned(
                //     _err_span,
                //     proc_macro_error::Level::Error,
                //     "expected `builder(each = \"...\")`".to_owned(),
                // )
                // .emit()
                let err_msg =
                    syn::Error::new_spanned(attr_meta, "expected `builder(each = \"...\")`")
                        .to_compile_error();
                return Some(Err(err_msg));
            }
        }
    }

    None
}

fn generate_impl_block(
    original_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
    original_ident: &syn::Ident,
    builder_struct_name: &syn::Ident,
) -> proc_macro2::TokenStream {
    let fields = original_fields.into_iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();

        if try_parse_attr_each(field).is_some() {
            quote! {
                #ident: std::vec::Vec::new()
            }
        } else {
            quote! {
                #ident: std::option::Option::None
            }
        }
    });

    quote! {
        impl #original_ident {
            pub fn builder() -> #builder_struct_name {
                #builder_struct_name {
                    #(#fields,)*
                }
            }
        }
    }
}

fn generate_builder_fields(
    original_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    original_fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        if is_type_option(ty).is_some() || try_parse_attr_each(field).is_some() {
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: std::option::Option::<#ty>
            }
        }
    })
}

fn generate_builder_methods(
    original_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    original_fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        let mut build_all_at_once = true;

        // Generate the one-at-a-time method
        let quote_part1 = match try_parse_attr_each(field) {
            Some(Ok(method_name)) => {
                let method_name: syn::Ident = method_name.parse().unwrap();
                if let Some(generic_type) = get_generic_type(ty) {
                    build_all_at_once = ident.to_string() != method_name.to_string();
                    quote! {
                        fn #method_name(&mut self, item: #generic_type) -> &mut Self {
                            self.#ident.push(item);
                            self
                        }
                    }
                } else {
                    Diagnostic::spanned(
                        ty.span(),
                        proc_macro_error::Level::Error,
                        "Could not determine the generic type".to_owned(),
                    )
                    .emit();
                    quote! {}
                }
            }
            Some(Err(ts)) => ts,
            _ => quote! {},
        };

        if !build_all_at_once {
            return quote_part1;
        }

        // Generate the all-at-once method
        let quote_part2 = match is_type_option(ty) {
            Some(generic_type) => quote! {
                fn #ident(&mut self, #ident: #generic_type) -> &mut Self{
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            },
            None => {
                if try_parse_attr_each(field).is_some() {
                    quote! {
                        fn #ident(&mut self, #ident: #ty) -> &mut Self{
                            self.#ident = #ident;
                            self
                        }
                    }
                } else {
                    quote! {
                        fn #ident(&mut self, #ident: #ty) -> &mut Self{
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                    }
                }
            }
        };

        quote! {
            #quote_part1
            #quote_part2
        }
    })
}

fn generate_build_method(
    original_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
    original_ident: &syn::Ident,
) -> proc_macro2::TokenStream {
    let build_fields = original_fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        if is_type_option(ty).is_some() || try_parse_attr_each(field).is_some() {
            quote! {
                #ident: self.#ident.clone()
            }
        } else {
            let err_msg = format!("{ident} is not set!");
            quote! {
                #ident: self.#ident.clone().ok_or(#err_msg)?
            }
        }
    });

    quote! {
        pub fn build(&self) -> std::result::Result<#original_ident, std::boxed::Box<dyn std::error::Error>> {
            Ok(#original_ident{
                #(#build_fields ,)*
            })
        }
    }
}

#[proc_macro_error]
#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    // Extract the struct's data
    let data_struct = match &input.data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => unimplemented!("derive(Builder) can only be applied to structs"),
    };

    // Extract the struct's fields
    let original_fields = match data_struct.fields {
        syn::Fields::Named(syn::FieldsNamed { ref named, .. }) => named,
        _ => unimplemented!("derive(Builder) can only be applied to structs with named fields"),
    };

    let original_ident = input.ident;
    let builder_struct_name = format_ident!("{}Builder", original_ident);

    let impl_block = generate_impl_block(original_fields, &original_ident, &builder_struct_name);

    let builder_fields = generate_builder_fields(original_fields);

    let builder_methods = generate_builder_methods(original_fields);

    let build_method = generate_build_method(original_fields, &original_ident);

    // Create the builder struct
    let doc_string = format!("Implements the builder pattern for [`{original_ident}`]");
    let doc = quote! {
        #[doc = #doc_string]
    };
    let builder_struct = quote! {
        #doc
        pub struct #builder_struct_name {
            #(#builder_fields,)*
        }
    };

    let builder_impl = quote! {
        impl #builder_struct_name {
            #(#builder_methods)*
            #build_method
        }
    };

    // Build the output, possibly using quasi-quotation
    let expanded = quote! {
        #impl_block
        #builder_struct
        #builder_impl
    };

    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}
