use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse::ParseStream;
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, Item};

struct SortedInput {
    item: syn::Item,
}

impl syn::parse::Parse for SortedInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(SortedInput {
            item: input.parse()?,
        })
    }
}

fn process_input(_args: TokenStream, input: syn::Item) -> syn::Result<()> {
    // Assert that the input is an enum
    let input_enum = if let Item::Enum(ref e) = input {
        e
    } else {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        ));
    };

    let mut current_iter = input_enum.variants.clone().into_iter();
    let _ = current_iter.next();
    let prev_iter = input_enum.variants.clone().into_iter();
    if let Some((variant_out_of_place, _)) = current_iter
        .zip(prev_iter)
        .find(|(current, prev)| current.ident < prev.ident)
    {
        for variant in input_enum.clone().variants {
            if variant.ident > variant_out_of_place.ident {
                return Err(syn::Error::new(
                    variant_out_of_place.span(),
                    format!(
                        "{} should sort before {}",
                        variant_out_of_place.ident, variant.ident
                    ),
                ));
            }
        }
    }

    Ok(())
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut original_input = input.clone();

    let _ = args;
    let SortedInput { item } = parse_macro_input!(input as SortedInput);

    return match process_input(args, item) {
        Ok(_) => original_input,
        Err(error) => {
            original_input.extend(TokenStream::from(error.to_compile_error()).into_iter());
            original_input
        }
    };
}

struct ConcatenatedPath {
    string: String,
    tokens: proc_macro2::TokenStream,
}

struct SortMatchVisitor {
    sort_errors: Vec<syn::Error>,
}

impl SortMatchVisitor {
    fn check_arm_supported(arm: &syn::Arm) -> syn::Result<()> {
        match arm.pat {
            syn::Pat::Path(_)
            | syn::Pat::Struct(_)
            | syn::Pat::TupleStruct(_)
            | syn::Pat::Ident(_)
            | syn::Pat::Wild(_) => Ok(()),
            _ => Err(syn::Error::new_spanned(
                arm.pat.clone(),
                "unsupported by #[sorted]",
            )),
        }
    }

    /// Concatenate the paths of the arm into a single string
    ///
    /// Returns the concatenated string and the pattern token stream
    fn get_arm_pat(arm: &syn::Arm) -> Option<ConcatenatedPath> {
        let get_complete_path_str = |path: &syn::Path| {
            let mut path_str = String::from("");

            let mut segments_iter = path.segments.iter();
            if let Some(segment) = segments_iter.next() {
                path_str.push_str(segment.ident.to_string().as_str());
            }

            while let Some(segment) = segments_iter.next() {
                path_str.push_str(format!("::{}", segment.ident).as_str());
            }

            path_str
        };

        match arm.pat {
            syn::Pat::Path(ref pat_path) => Some(ConcatenatedPath {
                string: get_complete_path_str(&pat_path.path),
                tokens: pat_path.path.to_token_stream(),
            }),
            syn::Pat::Struct(ref pat_struct) => Some(ConcatenatedPath {
                string: get_complete_path_str(&pat_struct.path),
                tokens: pat_struct.path.to_token_stream(),
            }),
            syn::Pat::TupleStruct(ref pat_tuple_struct) => Some(ConcatenatedPath {
                string: get_complete_path_str(&pat_tuple_struct.path),
                tokens: pat_tuple_struct.path.to_token_stream(),
            }),
            syn::Pat::Ident(ref pat_ident) => Some(ConcatenatedPath {
                string: pat_ident.ident.to_string(),
                tokens: pat_ident.ident.clone().into_token_stream(),
            }),
            syn::Pat::Wild(ref pat_wild) => Some(ConcatenatedPath {
                string: "_".to_owned(),
                tokens: pat_wild.into_token_stream(),
            }),
            _ => None,
        }
    }

    /// Compare 2 match arms and check if the current match arm should come before
    /// the previous match arm, in which case return the patterns of the match arms
    fn is_arm_out_of_place(
        current: &syn::Arm,
        prev: &syn::Arm,
    ) -> Option<(ConcatenatedPath, ConcatenatedPath)> {
        if let (Some(current_path), Some(prev_path)) =
            (Self::get_arm_pat(current), Self::get_arm_pat(prev))
        {
            if prev_path.string == "_" {
                return Some((current_path, prev_path));
            }
            if current_path.string < prev_path.string {
                return Some((current_path, prev_path));
            }
        }

        None
    }

    fn check_arms_sorted(arms: &Vec<syn::Arm>) -> syn::Result<()> {
        // Check for any unsupported arm patterns
        for arm in arms {
            Self::check_arm_supported(arm)?
        }

        // Check the sortedness of the arms
        let mut current_iter = arms.iter();
        let _ = current_iter.next();
        let prev_iter = arms.iter();
        if let Some((arm_out_of_place, _)) = current_iter
            .zip(prev_iter)
            .find(|(current, prev)| Self::is_arm_out_of_place(current, prev).is_some())
        {
            for arm in arms {
                if let Some((arm_out_of_place_pat, arm_pat)) =
                    Self::is_arm_out_of_place(arm_out_of_place, arm)
                {
                    return Err(syn::Error::new_spanned(
                        arm_out_of_place_pat.tokens,
                        format!(
                            "{} should sort before {}",
                            arm_out_of_place_pat.string, arm_pat.string
                        ),
                    ));
                }
            }
        }

        Ok(())
    }
}

impl syn::visit_mut::VisitMut for SortMatchVisitor {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        if let Some((index, _)) = node.attrs.iter().enumerate().find(|(_, attr)| {
            attr.path.segments.len() == 1 && attr.path.segments[0].ident == "sorted"
        }) {
            // Found a #[sorted] attribute on a match statement

            // Remove the #[sorted] attribute from the token stream
            // so that the stable compiler doesn't error out
            node.attrs.remove(index);

            // Analyze the match arms
            if let Err(error) = Self::check_arms_sorted(&node.arms) {
                self.sort_errors.push(error);
            }
        }

        // Delegate to the default impl to visit any nested match expressions
        syn::visit_mut::visit_expr_match_mut(self, node);
    }
}

// TODO: find a better way to communicate the sort errors to the caller
fn process_check(input: TokenStream) -> Result<TokenStream, TokenStream> {
    // Parse the decorated function
    let mut decorated_fn = match syn::parse::<syn::ItemFn>(input) {
        Ok(item_fn) => item_fn,
        Err(error) => return Err(TokenStream::from(error.to_compile_error())),
    };

    println!("{decorated_fn:#?}");

    // Use the visitor pattern to visit all match expressions inside the decorated function
    // and process those decorated with the #[sorted] attribute
    let mut sort_match_visitor = SortMatchVisitor {
        sort_errors: Vec::new(),
    };
    sort_match_visitor.visit_item_fn_mut(&mut decorated_fn);

    if !sort_match_visitor.sort_errors.is_empty() {
        let mut result = TokenStream::from(decorated_fn.into_token_stream());
        for error in sort_match_visitor.sort_errors {
            result.extend(TokenStream::from(error.to_compile_error()).into_iter());
        }
        return Err(result);
    }

    Ok(decorated_fn.into_token_stream().into())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;

    return match process_check(input) {
        Ok(ts) => ts,
        Err(error) => error,
    };
}
