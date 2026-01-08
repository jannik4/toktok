mod lexer;
mod parser;
mod rules;
mod use_statements;

use crate::{CompileError, ast, config::Config};
use proc_macro2::TokenStream;
use quote::quote;

pub fn generate(
    ast: ast::Ast<'_>,
    config: Config<'_>,
    error_sink: &mut impl FnMut(CompileError),
) -> TokenStream {
    // Generate use statements
    let use_statements = use_statements::generate(&ast, error_sink);

    // Generate lexer
    let (token_map, lexer) = lexer::generate(&ast, &config, error_sink);

    // Generate rules
    let rules = rules::generate(&ast, &token_map, error_sink);

    // Generate parser
    let parser = parser::generate(&ast, error_sink);

    quote! {
        mod parser {
            #![allow(unreachable_code)] // Required for loop placeholders

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
    }
}
