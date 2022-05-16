#![deny(rust_2018_idioms)]

mod ast;
mod generator;
mod lexer;
mod parser;
mod pretty_print;
mod token_map;

use anyhow::{Context, Result};
use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use std::{env, fs, path::PathBuf};

#[proc_macro]
pub fn make_parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse input
    let (path, source) = parse_input(input.into());

    // Parse grammar
    let (rust_source, ast, token_map) = match parse_grammar(&source) {
        Ok(res) => res,
        Err(e) => panic!("{}", e),
    };

    // Generate parser
    let parser_mod = match generator::generate(rust_source, ast, token_map) {
        Ok(res) => res,
        Err(e) => panic!("{}", e),
    };
    let parser_mod = parser_mod.parse::<TokenStream>().unwrap();

    // Recompile on change
    let recompile_on_change = {
        let path = path.to_str().unwrap();
        quote! {
            const _: &str = include_str!(#path); // Recompile if grammar changes
        }
    };

    (quote! {
        #recompile_on_change
        #parser_mod
    })
    .into()
}

fn parse_input(input: TokenStream) -> (PathBuf, String) {
    let mut items = input.into_iter();

    // grammar
    if !matches!(items.next().unwrap(), TokenTree::Ident(ident) if ident == "grammar") {
        panic!("expected `grammar = \"<PATH>\"`");
    }

    // =
    if !matches!(items.next().unwrap(), TokenTree::Punct(punct) if punct.as_char() == '=') {
        panic!("expected `grammar = \"<PATH>\"`");
    }

    // grammar path
    let root_path =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string()));
    let path = match items.next().unwrap() {
        TokenTree::Literal(lit) => {
            let lit = lit.to_string();
            if lit.starts_with('"') && lit.ends_with('"') {
                root_path.join(&lit[1..lit.len() - 1])
            } else {
                panic!("expected `grammar = \"<PATH>\"`")
            }
        }
        _ => panic!("expected `grammar = \"<PATH>\"`"),
    };

    // Source
    let source = fs::read_to_string(&path).expect("failed to read parser from file");

    (path, source)
}

fn parse_grammar(source: &str) -> Result<(&str, ast::Ast<'_>, token_map::TokenMap)> {
    // Split into parts
    let (parser_source, token_map_source, rust_source) = split_into_parts(&source)?;

    // Build ast
    let ast = match parser::parse(parser_source) {
        Ok(ast) => ast,
        Err(error) => {
            return Err(pretty_print::pretty_print_toktok_error(error, parser_source));
        }
    };

    // Build token map
    let token_map = token_map::parse_and_build(token_map_source)?;

    Ok((rust_source, ast, token_map))
}

fn split_into_parts(source: &str) -> Result<(&str, &str, &str)> {
    let mut parts = source.split("+++");

    let part0 = parts.next().context("expected 3 parts")?;
    let part1 = parts.next().context("expected 3 parts")?;
    let part2 = parts.next().context("expected 3 parts")?;

    Ok((part0, part1, part2))
}
