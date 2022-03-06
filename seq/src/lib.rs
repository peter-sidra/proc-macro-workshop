use proc_macro2::{Group, Punct, TokenStream, TokenTree};
use quote::{format_ident, TokenStreamExt};
use syn::{
    braced, parse::Parse, parse_macro_input, spanned::Spanned, Expr, ExprLit, ExprRange, Ident,
    Lit, RangeLimits, Token,
};

// Detects the repeat pattern #()*
fn is_repeat_pattern(punct: &Punct, rest: &proc_macro2::token_stream::IntoIter) -> Option<Group> {
    if punct.as_char() == '#' {
        let mut iter = rest.clone();
        if let Some(TokenTree::Group(group)) = iter.next() {
            if matches!(iter.next(), Some(TokenTree::Punct(punct)) if punct.as_char() == '*') {
                return Some(group);
            }
        }
    }

    None
}

// Detects the unified ident pattern [ident][~][ident]
// returns the unified pattern and the number of consumed tokens
fn is_unified_ident(
    ident: &Ident,
    repeat_ident: &Ident,
    index: i32,
    rest: &proc_macro2::token_stream::IntoIter,
) -> Option<(Ident, usize)> {
    let mut rest_clone = rest.clone();
    if let Some(TokenTree::Punct(punct)) = rest_clone.next() {
        if punct.as_char() == '~' {
            if let Some(TokenTree::Ident(ref next_ident)) = rest_clone.next() {
                if repeat_ident == next_ident {
                    // TODO: check for a suffix (another [~][N]) pattern

                    // We matched the [ident][~][N] pattern
                    // Create new unified ident
                    let literal = TokenTree::Literal(proc_macro2::Literal::i32_unsuffixed(index));
                    let mut new_ident = format_ident!("{ident}{literal}");
                    new_ident.set_span(ident.span());

                    return Some((new_ident, 2));
                }
            }
        }
    }

    None
}

#[derive(Debug)]
struct SeqRange {
    ident: Ident,
    range: std::ops::Range<i32>,
}

impl Parse for SeqRange {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let range: ExprRange = input.parse()?;

        let parse_lit_int = |expr: &Option<Box<Expr>>| {
            if let Some(inner) = expr {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Int(ref value),
                    ..
                }) = inner.as_ref()
                {
                    return value.base10_parse::<i32>();
                } else {
                    return Err(syn::Error::new(
                        inner.as_ref().span(),
                        r#"Expected valid int literal"#,
                    ));
                }
            }
            Err(syn::Error::new(
                range.span(),
                r#"Expected valid rust range "start..end""#,
            ))
        };

        let from = parse_lit_int(&range.from)?;
        let to = parse_lit_int(&range.to)?;

        let range = match range.limits {
            RangeLimits::HalfOpen(_) => from..to,
            RangeLimits::Closed(_) => from..to + 1,
        };

        Ok(Self { ident, range })
    }
}

struct SeqInput {
    seq_range: SeqRange,
    body: TokenStream,
    sectioned: bool,
}

impl SeqInput {
    fn contains_repeat(body: TokenStream) -> bool {
        let mut iter = body.into_iter();
        while let Some(tt) = iter.next() {
            let result = match tt {
                TokenTree::Group(group) => Self::contains_repeat(group.stream()),
                TokenTree::Punct(ref punct) => is_repeat_pattern(punct, &iter).is_some(),
                _ => false,
            };
            if result == true {
                return result;
            }
        }
        false
    }
}

impl Parse for SeqInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let seq_range = input.parse()?;

        let body_ts;
        braced!(body_ts in input);
        let body: TokenStream = body_ts.parse()?;

        let sectioned = Self::contains_repeat(body.clone());
        Ok(Self {
            seq_range,
            body,
            sectioned,
        })
    }
}

struct Expander {
    ident: Ident,
    range: std::ops::Range<i32>,
    sectioned: bool,
}

impl Expander {
    fn process_tt(
        &self,
        tt: TokenTree,
        index: i32,
        ts_iter: &mut proc_macro2::token_stream::IntoIter,
    ) -> TokenTree {
        match tt {
            TokenTree::Group(ref group) => {
                let mut expanded_group =
                    Group::new(group.delimiter(), self.process_ts(group.stream(), index));
                expanded_group.set_span(group.span());

                TokenTree::Group(expanded_group)
            }
            TokenTree::Ident(ref ident) => {
                if &self.ident == ident {
                    let mut literal =
                        TokenTree::Literal(proc_macro2::Literal::i32_unsuffixed(index));
                    literal.set_span(ident.span());
                    return literal;
                } else {
                    if let Some((new_ident, num_of_consumed_tokens)) =
                        is_unified_ident(ident, &self.ident, index, ts_iter)
                    {
                        // Advance the iterator past the consumed tokens
                        ts_iter.nth(num_of_consumed_tokens - 1);

                        return TokenTree::Ident(new_ident);
                    }
                }
                tt
            }
            _ => tt,
        }
    }

    fn process_ts(&self, ts: TokenStream, index: i32) -> TokenStream {
        let mut out_ts = TokenStream::new();
        let mut ts_iter = ts.into_iter();

        while let Some(tt) = ts_iter.next() {
            out_ts.append(self.process_tt(tt, index, &mut ts_iter));
        }

        out_ts
    }

    fn expand_section(&self, ts: TokenStream) -> TokenStream {
        self.range
            .clone()
            .map(|i| self.process_ts(ts.clone(), i))
            .collect()
    }

    fn expand(&self, ts: TokenStream) -> TokenStream {
        if !self.sectioned {
            return self.expand_section(ts);
        }

        let mut out_ts = TokenStream::new();
        let mut ts_iter = ts.into_iter();

        while let Some(tt) = ts_iter.next() {
            match tt {
                TokenTree::Group(ref group) => {
                    // Recurse down the group
                    let mut expanded_group =
                        Group::new(group.delimiter(), self.expand(group.stream()));

                    expanded_group.set_span(group.span());

                    out_ts.append(TokenTree::Group(expanded_group));
                }
                TokenTree::Punct(ref punct) => {
                    if let Some(group) = is_repeat_pattern(punct, &ts_iter) {
                        // Matched the repetition pattern
                        out_ts.extend(self.expand_section(group.stream()));

                        // Advance the token stream iterator past the *
                        ts_iter.next();
                        ts_iter.next();

                        continue;
                    }
                    out_ts.append(tt);
                }
                _ => out_ts.append(tt),
            };
        }

        out_ts
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let SeqInput {
        seq_range,
        body,
        sectioned,
    } = parse_macro_input!(input as SeqInput);

    let expander = Expander {
        ident: seq_range.ident,
        range: seq_range.range,
        sectioned,
    };

    let result = expander.expand(body);

    // println!("Result: {result}");
    result.into()
}
