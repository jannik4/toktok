#![deny(rust_2018_idioms)]

mod ast;
mod config;
mod generator;
mod lexer;
mod parser;
mod pretty_print;

use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use std::{env, fs, path::PathBuf};

#[proc_macro]
pub fn make_parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse input
    let (path, source) = parse_input(input.into());

    // Recompile on change
    let recompile_on_change = {
        let path = path.to_str().expect("invalid path");
        quote! {
            const _: &str = include_str!(#path); // Recompile if grammar changes
        }
    };

    // Parse grammar
    let ast = match parser::parse(&source) {
        Ok(res) => res,
        Err(e) => panic!("{}", e),
    };

    // Build config
    let config = config::Config::build(&ast);

    // Generate parser
    let gen_tokens = match generator::generate(ast, config) {
        Ok(res) => res,
        Err(e) => panic!("{}", e),
    };

    (quote! {
        #recompile_on_change
        #gen_tokens
    })
    .into()
}

fn parse_input(input: TokenStream) -> (PathBuf, String) {
    let mut items = input.into_iter();

    // grammar
    if !matches!(items.next(), Some(TokenTree::Ident(ident)) if ident == "grammar") {
        panic!("expected `grammar = \"<PATH>\"`");
    }

    // =
    if !matches!(items.next(), Some( TokenTree::Punct(punct)) if punct.as_char() == '=') {
        panic!("expected `grammar = \"<PATH>\"`");
    }

    // grammar path
    let root_path =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string()));
    let path = match items.next() {
        Some(TokenTree::Literal(lit)) => {
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
