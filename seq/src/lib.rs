#![recursion_limit = "128"]
extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Result, Token};

#[derive(Debug)]
struct SeqMacroInput {
    from: syn::LitInt,
    to: syn::LitInt,
    inclusive: bool,
    ident: syn::Ident,
    tt: proc_macro2::TokenStream,
}

// seq!(N #ident in 1..3 #from..#to {
//     println!("{}", i); #tt
// });

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = syn::Ident::parse(input)?;
        let _in = <Token![in]>::parse(input)?;
        let from = syn::LitInt::parse(input)?;
        // peek ahead to the next token and see if there is an `=` sign
        let inclusive = input.peek(Token![..=]);
        if inclusive {
            <Token![..=]>::parse(input)?;
        } else {
            <Token![..]>::parse(input)?;
        }
        let to = syn::LitInt::parse(input)?;
        let content;
        let _braces = syn::braced!(content in input);
        let tt = proc_macro2::TokenStream::parse(&content)?;
        // eprintln!("{:?}", tt);

        Ok(SeqMacroInput {
            from,
            to,
            inclusive,
            ident,
            tt,
        })
    }
}

impl From<SeqMacroInput> for proc_macro2::TokenStream {
    fn from(input: SeqMacroInput) -> Self {
        input.expand(input.tt.clone())
    }
}

#[derive(Clone, Copy, Debug)]
enum Mode {
    ReplaceIdent(u64),
    ReplaceSequence,
}

impl SeqMacroInput {
    // helper function for returning iterator based on if the macro input is inclusive or not
    fn range(&self) -> impl Iterator<Item = u64> {
        if self.inclusive {
            self.from.value()..(self.to.value() + 1)
        } else {
            self.from.value()..self.to.value()
        }
    }

    // replace individual idents over specified range
    fn replace_idents(&self, tts: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        self.range()
            .map(|i| self.expand_pass(tts.clone(), Mode::ReplaceIdent(i)))
            .map(|(ts, _)| ts)
            .collect()
    }

    fn expand2(
        &self,
        tt: proc_macro2::TokenTree,
        rest: &mut proc_macro2::token_stream::IntoIter,
        mutated: &mut bool,
        mode: Mode,
    ) -> proc_macro2::TokenStream {
        let tt = match tt {
            // A token tree surrounded by delimiters (`()`, `{}`, `[]`) and needs to be expanded
            proc_macro2::TokenTree::Group(g) => {
                // expand the group
                let (expanded, g_mutated) = self.expand_pass(g.stream(), mode);
                // create new group of the newly expanded token stream
                let mut expanded = proc_macro2::Group::new(g.delimiter(), expanded);
                // set mutated if mutauted or g_mutated is true
                *mutated |= g_mutated;
                // retain the span from the the groupd and set it to the now expanded Group
                expanded.set_span(g.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            // if from this token tree it is an Ident AND this Ident equals the one parsed
            // into SeqMacroInput. In this case `if N == N`
            // self.ident could be `i`, `N`, `x`, etc.
            proc_macro2::TokenTree::Ident(ref ident) if ident == &self.ident => {
                // if we need to replace the individual ident
                if let Mode::ReplaceIdent(i) = mode {
                    // create u64 literal with the current value
                    let mut lit = proc_macro2::Literal::u64_unsuffixed(i);
                    // retain the span from the Ident `N` and set it to the u64
                    lit.set_span(ident.span());
                    *mutated = true;
                    proc_macro2::TokenTree::Literal(lit)
                } else {
                    // no op
                    proc_macro2::TokenTree::Ident(ident.clone())
                }
            }
            // "else" case for Ident where the ident does NOT equal self.ident in token stream
            proc_macro2::TokenTree::Ident(mut ident) => {
                // search for ~ followed by self.ident at the end of an identifier
                // OR ~ self.ident ~
                let mut peek = rest.clone();
                match (mode, peek.next(), peek.next()) {
                    (
                        // replace individual ident
                        Mode::ReplaceIdent(i),
                        // if the NEXT token is SOME and also is a punct that is `~`
                        Some(proc_macro2::TokenTree::Punct(ref punct)),
                        // check peek's next next is SOME and also and see if it's an Ident => f~N searching for N
                        Some(proc_macro2::TokenTree::Ident(ref ident2)),
                    ) if punct.as_char() == '~' && ident2 == &self.ident => {
                        // mutate the original ident to go from f~N to f3
                        // ident: f, punct: ~, ident2: N
                        ident = proc_macro2::Ident::new(&format!("{}{}", ident, i), ident.span());
                        // now set rest to be the current index and go from f~N to f~N
                        //                                                  ^        ^
                        *rest = peek.clone();
                        *mutated = true;

                        // we may also consume another ~
                        // that way f~N and f~N~ work when fully expanded
                        match peek.next() {
                            Some(proc_macro2::TokenTree::Punct(ref punct))
                                if punct.as_char() == '~' =>
                            {
                                *rest = peek.clone();
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                // return newly mutated ident: f~N => f3
                proc_macro2::TokenTree::Ident(ident)
            }
            proc_macro2::TokenTree::Punct(ref p) if p.as_char() == '#' => {
                if let Mode::ReplaceSequence = mode {
                    // is this #(...)* ?
                    let mut peek = rest.clone();
                    match (peek.next(), peek.next()) {
                        (
                            Some(proc_macro2::TokenTree::Group(ref rep)),
                            Some(proc_macro2::TokenTree::Punct(ref star)),
                        ) if rep.delimiter() == proc_macro2::Delimiter::Parenthesis
                            && star.as_char() == '*' =>
                        {
                            // rep: `(...)`, star: `*`
                            // expand ... for each sequence in the range
                            *mutated = true;
                            *rest = peek;

                            // replace ... with individual idents over the given range
                            return self.replace_idents(rep.stream());
                        }
                        _ => {}
                    }
                }
                proc_macro2::TokenTree::Punct(p.clone())
            }
            tt => tt, // no op
        };
        std::iter::once(tt).collect()
    }

    /// `expand_pass` applies `expand2` to each token tree in a token stream and
    /// returns the expanded token stream along with a boolean flag to indicate whether any mutation occurred.
    fn expand_pass(
        &self,
        stream: proc_macro2::TokenStream,
        mode: Mode,
    ) -> (proc_macro2::TokenStream, bool) {
        let mut out = proc_macro2::TokenStream::new();
        let mut mutated = false;
        let mut tts = stream.into_iter();
        while let Some(tt) = tts.next() {
            // for every tts in the iter, expand it and mutate if necessary
            out.extend(self.expand2(tt, &mut tts, &mut mutated, mode));
        }
        (out, mutated)
    }

    fn expand(&self, stream: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let (out, mutated) = self.expand_pass(stream.clone(), Mode::ReplaceSequence);
        // if true return the out token stream because all necessary mutations would've already happened
        if mutated {
            return out;
        }

        // #(...)* wasn't found and now individual identifiers need to be replaced over this range
        self.replace_idents(stream)
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}
