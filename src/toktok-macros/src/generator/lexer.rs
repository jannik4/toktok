use crate::{ast, config::Config};
use anyhow::Result;
use proc_macro2::TokenStream;
use quote::quote;
use std::collections::{HashMap, HashSet};
use std::iter;

pub fn generate<'a>(
    ast: &ast::Ast<'a>,
    config: &Config<'_>,
) -> Result<(HashMap<ast::Token<'a>, usize>, TokenStream)> {
    // Get all tokens
    let tokens = tokens(&ast);

    // Token enum variants
    let token_variants = tokens.iter().enumerate().map(|(idx, token)| match token {
        ast::Token::TokenLit(t) => {
            format!(r#"#[token("{}")] Token{}"#, t.0, idx).parse::<TokenStream>().unwrap()
        }
        ast::Token::TokenRegex(t) => {
            format!(r#"#[regex("{}")] Token{}"#, t.0, idx).parse::<TokenStream>().unwrap()
        }
    });

    // Lexer skip
    let lexer_skip = config
        .lexer_skip
        .iter()
        .map(|token| format!(r#"#[logos(skip r"{}")]"#, token.0).parse::<TokenStream>().unwrap());

    // Token display
    let token_display = tokens
        .iter()
        .enumerate()
        .map(|(idx, token)| match token {
            ast::Token::TokenLit(t) => {
                format!(r#"Token::Token{} => write!(f, "`{{}}`", "{}")"#, idx, t.0)
                    .parse::<TokenStream>()
                    .unwrap()
            }
            ast::Token::TokenRegex(t) => {
                format!(r#"Token::Token{} => write!(f, "r`{{}}`", "{}")"#, idx, t.0)
                    .parse::<TokenStream>()
                    .unwrap()
            }
        })
        .chain(iter::once(quote! { Token::Error => write!(f, "ERROR") }));

    // Token map
    let token_map = tokens.iter().enumerate().map(|(idx, token)| (*token, idx)).collect();

    //
    Ok((
        token_map,
        quote! {
            mod __lexer__ {
                use ::toktok::__intern::logos::Logos;
                use ::std::fmt;

                #[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord,  Hash)]
                #[logos(crate = ::toktok::__intern::logos)]
                #(#lexer_skip)*
                pub enum Token {
                    #(#token_variants,)*

                    Error,
                }

                impl fmt::Display for Token {
                    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                        match self {
                            #(#token_display,)*
                        }
                    }
                }

                pub fn lex(source: &str) -> Vec<::toktok::SpannedToken<Token>> {
                    let mut lexer = Token::lexer(source);
                    let mut tokens = Vec::new();
                    while let Some(token_result) = lexer.next() {
                        let token = match token_result {
                            Ok(token) => token,
                            Err(()) => Token::Error,
                        };

                        tokens.push(::toktok::SpannedToken { token, span: lexer.span() });
                    }
                    tokens
                }
            }
        },
    ))
}

fn tokens<'a>(ast: &ast::Ast<'a>) -> Vec<ast::Token<'a>> {
    let mut tokens = HashSet::new();

    fn collect_tokens<'a>(combinator: &ast::Combinator<'a>, tokens: &mut HashSet<ast::Token<'a>>) {
        match combinator {
            ast::Combinator::Seq(c) => {
                for combinator in &c.combinators {
                    collect_tokens(combinator, tokens);
                }
            }
            ast::Combinator::Choice(c) => {
                for combinator in &c.combinators {
                    collect_tokens(combinator, tokens);
                }
            }
            ast::Combinator::Opt(c) => collect_tokens(&c.combinator, tokens),
            ast::Combinator::Many0(c) => collect_tokens(&c.combinator, tokens),
            ast::Combinator::Many1(c) => collect_tokens(&c.combinator, tokens),
            ast::Combinator::Atom(c) => match &c.kind {
                ast::CombinatorAtomKind::Token(t) => {
                    tokens.insert(*t);
                }
                ast::CombinatorAtomKind::Path(_) => (),
                ast::CombinatorAtomKind::FunctionCall(fc) => {
                    for combinator in &fc.args {
                        collect_tokens(combinator, tokens);
                    }
                }
            },
        }
    }

    for item in &ast.items {
        match item {
            ast::Item::UseStatement(_) | ast::Item::Config(_) => (),
            ast::Item::Rule(rule) => {
                for production in &rule.productions {
                    for combinator in &production.combinator.combinators {
                        collect_tokens(combinator, &mut tokens);
                    }
                }
            }
        }
    }

    // Sort tokens
    let mut tokens = tokens.into_iter().collect::<Vec<_>>();
    tokens.sort();

    tokens
}
