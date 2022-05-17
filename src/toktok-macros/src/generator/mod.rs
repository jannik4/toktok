mod lexer;
mod parser;
mod rules;
mod use_statements;

use crate::{ast, config::Config};
use anyhow::Result;
use proc_macro2::TokenStream;
use quote::quote;

pub fn generate(ast: ast::Ast<'_>, config: Config<'_>) -> Result<TokenStream> {
    // Generate use statements
    let use_statements = use_statements::generate(&ast)?;

    // Generate lexer
    let (token_map, lexer) = lexer::generate(&ast, &config)?;

    // Generate rules
    let rules = rules::generate(&ast, &token_map)?;

    // Generate parser
    let parser = parser::generate(&ast)?;

    Ok(quote! {
        mod parser {
            #![allow(non_snake_case)]
            #![allow(unused_braces)]
            #![allow(clippy::all)]
            #![deny(clippy::correctness)]

            // Use statements
            #use_statements

            // Export Token
            pub use self::__lexer__::Token;

            // Export parser
            #parser

            // Lexer (mod __lexer__ { Token, lex })
            #lexer

            // Rules (mod __rules__ { rules })
            #rules
        }
    })
}
