use crate::{ast, CompileError};
use proc_macro2::{TokenStream, TokenTree};
use quote::{format_ident, quote};
use std::{cmp::Ordering, collections::HashMap};

pub fn generate(
    ast: &ast::Ast<'_>,
    token_map: &HashMap<ast::Token<'_>, usize>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    // Generate rules
    let mut rules = TokenStream::new();
    for item in &ast.items {
        match item {
            ast::Item::UseStatement(_) | ast::Item::Config(_) => (),
            ast::Item::Rule(rule) => rules.extend(generate_rule(rule, token_map, error_sink)),
        }
    }

    quote! {
        mod __rules__ {
            #[allow(unused)] use super::*;
            #[allow(unused)] use ::toktok::Parser as _;

            #rules

            mod __intern__ {
                pub use ::toktok::{combinator as c, State, Input, Result, Parser, Error, ParserError};
            }
        }
    }
}

fn generate_rule(
    rule: &ast::Rule<'_>,
    token_map: &HashMap<ast::Token<'_>, usize>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    // Calc common combinators
    let common_combinators = common_combinators(rule);

    // Body of the rule fn
    let mut body = TokenStream::new();

    // Check if we need to calc span. If yes, generate __span_start__
    let calc_span = rule.productions.iter().any(|p| p.rust_expression.0.contains("$span"));
    if calc_span {
        body.extend(quote! {
            let __span_start__ = __state__.input().positioned_start();
        });
    }

    // Generate common combinators
    if common_combinators != 0 {
        for idx in 1..=common_combinators {
            let c_name = format_ident!("__c_{}__", idx);
            let combinator = generate_combinator(
                &rule.productions[0].combinator.combinators[idx - 1],
                token_map,
                error_sink,
            );

            body.extend(quote! {
                let (__state__, #c_name) = #combinator.parse(__state__)?;
            });
        }
    }

    // Generate productions
    for (idx, production) in rule.productions.iter().enumerate() {
        body.extend(generate_production_match_block(
            production,
            calc_span,
            idx == rule.productions.len() - 1,
            common_combinators,
            token_map,
            error_sink,
        ));
    }

    // Generate rule fn
    let rule_name = format_ident!("{}", rule.name.0);
    let rule_ret_type = match rule.ret_type.0.parse::<TokenStream>() {
        Ok(tokens) => tokens,
        Err(error) => {
            error_sink(error.into());
            quote! { () } // Placeholder
        }
    };
    quote! {
        pub fn #rule_name<'s, 't>(
            __state__: self::__intern__::State<'s, 't, Token>
        ) -> self::__intern__::Result<'s, 't, Token, #rule_ret_type> where 's: 't {
            #body
        }
    }
}

fn generate_production_match_block(
    production: &ast::Production<'_>,
    calc_span: bool,
    is_last: bool,
    skip_combinators: usize,
    token_map: &HashMap<ast::Token<'_>, usize>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    let is_fallible = production.is_fallible;

    let c_tuple = c_tuple(1 + skip_combinators, production.combinator.combinators.len());

    let rust_expr = generate_rust_expression(production, error_sink);
    let rust_expr = if is_fallible {
        // Closure is needed to ensure res_block errors are catched
        quote! {
            match (move || #rust_expr)() {
                Ok(__ok__) => Ok((__state__, __ok__)),
                Err(__e__) => Err(__state__.and_error(__e__)),
            }
        }
    } else {
        // Closure is needed to ensure res_block does not return any errors
        quote! { Ok::<_, ::core::convert::Infallible>((__state__, (move || #rust_expr)())) }
    };

    let production = generate_production(production, skip_combinators, token_map, error_sink);

    let calc_span = if calc_span {
        quote! {
            let __span_end__ = __state__.input().positioned_end(__span_start__);
            let __span__ = __span_start__..__span_end__;
        }
    } else {
        quote! {}
    };

    if is_last {
        let rust_expr = if is_fallible {
            quote!(#rust_expr)
        } else {
            quote! {
                match #rust_expr {
                    Ok(__ok__) => Ok(__ok__),
                    Err(infallible) => match infallible {},
                }
            }
        };

        quote! {
            match #production {
                Ok((__state__, #c_tuple)) => return {{
                    #calc_span
                    #rust_expr
                }},
                Err(__e__) => Err(__e__),
            }
        }
    } else {
        let rust_expr = if is_fallible {
            quote! {
                match #rust_expr {
                    Ok(__ok__) => return Ok(__ok__),
                    Err(__e__) => __e__.recover(__input__)?,
                }
            }
        } else {
            quote! {
                match #rust_expr {
                    Ok(__ok__) => return Ok(__ok__),
                    Err(infallible) => match infallible {},
                }
            }
        };

        quote! {
            let __state__ = {
                let __input__ = __state__.input();
                match #production {
                    Ok((__state__, #c_tuple)) => {
                        #calc_span
                        #rust_expr
                    },
                    Err(__e__) => __e__.recover(__input__)?,
                }
            };
        }
    }
}

fn generate_production(
    production: &ast::Production<'_>,
    skip_combinators: usize,
    token_map: &HashMap<ast::Token<'_>, usize>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    let mut combinators = production.combinator.combinators.iter().skip(skip_combinators);
    let mut output = match combinators.next() {
        Some(combinator) => generate_combinator(combinator, token_map, error_sink),
        None => return quote! { Ok::<_, self::__intern__::ParserError<Token>>((__state__, ())) },
    };

    for combinator in combinators {
        let c = generate_combinator(combinator, token_map, error_sink);
        output = quote! { self::__intern__::c::seq((#output, #c)) };
    }

    quote! { #output.parse(__state__) }
}

fn generate_combinator(
    combinator: &ast::Combinator<'_>,
    token_map: &HashMap<ast::Token<'_>, usize>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    match combinator {
        ast::Combinator::Seq(seq) => {
            let mut combinators = seq.combinators.iter();
            let init = match combinators.next() {
                Some(c) => generate_combinator(c, token_map, error_sink),
                None => {
                    error_sink(CompileError::from_message("seq with zero combinators"));
                    quote! { () } // Placeholder
                }
            };
            let pair_combinator = combinators.fold(init, |lhs, rhs| {
                let rhs = generate_combinator(rhs, token_map, error_sink);
                quote! { self::__intern__::c::seq((#lhs, #rhs)) }
            });

            if seq.combinators.len() > 2 {
                let flattened = flatten_c_tuple("__pair__", seq.combinators.len());
                quote! { #pair_combinator.map(|__pair__| #flattened) }
            } else {
                pair_combinator
            }
        }
        ast::Combinator::Choice(choice) => {
            let mut combinators = choice.combinators.iter();
            let init = match combinators.next() {
                Some(c) => generate_combinator(c, token_map, error_sink),
                None => {
                    error_sink(CompileError::from_message("choice with zero combinators"));
                    quote! { () } // Placeholder
                }
            };
            combinators.fold(init, |lhs, rhs| {
                let rhs = generate_combinator(rhs, token_map, error_sink);
                quote! { self::__intern__::c::alt((#lhs, #rhs)) }
            })
        }
        ast::Combinator::Opt(opt) => {
            let inner = generate_combinator(&opt.combinator, token_map, error_sink);
            quote! { self::__intern__::c::opt(#inner) }
        }
        ast::Combinator::Many0(many0) => {
            let inner = generate_combinator(&many0.combinator, token_map, error_sink);
            quote! { self::__intern__::c::many0(#inner) }
        }
        ast::Combinator::Many1(many1) => {
            let inner = generate_combinator(&many1.combinator, token_map, error_sink);
            quote! { self::__intern__::c::many1(#inner) }
        }
        ast::Combinator::Atom(atom) => match &atom.kind {
            ast::CombinatorAtomKind::Path(path) => match path.0.parse::<TokenStream>() {
                Ok(tokens) => tokens,
                Err(error) => {
                    error_sink(error.into());
                    quote! { () } // Placeholder
                }
            },
            ast::CombinatorAtomKind::Token(token) => {
                let token_name = format_ident!("Token{}", token_map.get(token).unwrap());
                quote! {
                    self::__intern__::c::exact(super::__lexer__::Token::#token_name)
                }
            }
            ast::CombinatorAtomKind::FunctionCall(function_call) => {
                let name = format_ident!("{}", function_call.name.0);
                let args = function_call
                    .args
                    .iter()
                    .map(|arg| generate_combinator(arg, token_map, error_sink))
                    .collect::<Vec<_>>();

                quote! { #name(#(#args),*) }
            }
        },
    }
}

fn generate_rust_expression(
    production: &ast::Production<'_>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    let mut res = production.rust_expression.0.replace("$span", "__span__");

    while let Some(start) = res.find("$") {
        let end = res[start + 1..]
            .find(|c: char| !c.is_ascii_digit() && c != '_')
            .map(|end| start + end + 1)
            .unwrap_or_else(|| res.len());

        let name = &res[start..end];
        let combinator = match res[start + 1..end].parse::<usize>() {
            Ok(idx) if idx > 0 && idx <= production.combinator.combinators.len() => {
                format!("__c_{}__", idx)
            }
            _ => {
                error_sink(CompileError::from_message(format!(
                    "invalid index in rust expression: {}",
                    name
                )));
                "{#[allow(clippy::infinite_loop)] loop {}}".to_string()
            }
        };

        res.replace_range(start..end, &combinator);
    }

    match res.parse() {
        Ok(tokens) => tokens,
        Err(error) => {
            error_sink(error.into());
            // Placeholder
            quote! {{
                #[allow(clippy::infinite_loop)]
                loop {}
            }}
        }
    }
}

fn c_tuple(from: usize, to: usize) -> TokenStream {
    match from.cmp(&to) {
        Ordering::Greater => quote! { () },
        Ordering::Equal => {
            let c = format_ident!("__c_{}__", to);
            quote! { #c }
        }
        Ordering::Less => {
            let rest = c_tuple(from, to - 1);
            let c = format_ident!("__c_{}__", to);
            quote! { (#rest, #c) }
        }
    }
}

// Flatten tuple
// (((a, b), c), d) => (tuple.0.0.0, tuple.0.0.1, tuple.0.1, tuple.1)
fn flatten_c_tuple(name: &str, len: usize) -> TokenStream {
    let elements = (0..len).map(|idx| {
        let mut e = TokenStream::from(TokenTree::Ident(format_ident!("{}", name)));
        for _ in 0..(len - idx - 1) {
            e.extend(quote! { .0 });
        }
        if idx != 0 {
            e.extend(quote! { .1 });
        }
        e
    });

    quote! { (#(#elements),*) }
}

fn common_combinators(rule: &ast::Rule<'_>) -> usize {
    if rule.productions.iter().any(|p| p.is_fallible) {
        return 0;
    }

    // Calc common combinators
    let mut n = 0;
    while let Some(c) = rule.productions[0].combinator.combinators.get(n) {
        for prod in rule.productions.iter().skip(1) {
            match prod.combinator.combinators.get(n) {
                Some(c2) if c == c2 => (),
                _ => return n,
            }
        }
        n += 1;
    }
    n
}
