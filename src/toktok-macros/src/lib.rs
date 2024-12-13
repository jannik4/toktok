#![deny(rust_2018_idioms)]
#![deny(clippy::expect_used, clippy::panic)]

mod ast;
mod config;
mod error;
mod generator;
mod lexer;
mod parser;

use self::error::CompileError;
use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use std::{env, fs, path::PathBuf};

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[proc_macro]
pub fn make_parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse input
    let (source, recompile_on_change) = match parse_input(input.into()) {
        Ok((source, recompile_on_change)) => (source, recompile_on_change),
        Err(e) => return quote!(#e).into(),
    };

    let (tokens, errors) = build(&source);
    let tokens = quote! {
        #(#errors)*

        #recompile_on_change
        #tokens
    };

    tokens.into()
}

fn parse_input(input: TokenStream) -> Result<(String, TokenStream), CompileError> {
    let mut items = input.into_iter();

    // grammar
    if !matches!(items.next(), Some(TokenTree::Ident(ident)) if ident == "grammar") {
        return Err(CompileError::from_message("expected `grammar = \"<PATH>\"`"));
    }

    // =
    if !matches!(items.next(), Some( TokenTree::Punct(punct)) if punct.as_char() == '=') {
        return Err(CompileError::from_message("expected `grammar = \"<PATH>\"`"));
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
                return Err(CompileError::from_message("expected `grammar = \"<PATH>\"`"));
            }
        }
        _ => return Err(CompileError::from_message("expected `grammar = \"<PATH>\"`")),
    };

    // Source
    let source = match fs::read_to_string(&path) {
        Ok(source) => source,
        Err(e) => return Err(CompileError::from_message(e.to_string())),
    };

    // Recompile on change
    let path = match path.to_str() {
        Some(path) => path,
        None => return Err(CompileError::from_message("invalid path")),
    };
    let recompile_on_change = quote! {
        const _: &str = include_str!(#path); // Recompile if grammar changes
    };

    Ok((source, recompile_on_change))
}

fn build(source: &str) -> (Option<TokenStream>, Vec<CompileError>) {
    // Parse grammar
    let ast = match parser::parse(source) {
        Ok(ast) => ast,
        Err(e) => return (None, vec![e]),
    };

    let mut errors = Vec::new();
    let mut error_sink = |e| errors.push(e);

    // Build config
    let config = config::Config::build(&ast, &mut error_sink);

    // Generate parser
    let tokens = generator::generate(ast, config, &mut error_sink);

    (Some(tokens), errors)
}
