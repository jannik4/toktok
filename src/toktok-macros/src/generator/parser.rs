use crate::ast;
use anyhow::Result;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub fn generate(ast: &ast::Ast<'_>) -> Result<TokenStream> {
    let parser = ast
        .items
        .iter()
        .filter_map(|item| match item {
            ast::Item::Rule(rule) if rule.is_public => {
                let name = format_ident!("{}", rule.name.0);
                let ret_type = rule.ret_type.0.parse::<TokenStream>().expect("failed to parse return type");

                Some(quote! {
                    pub fn #name(source: &str) -> Result<#ret_type, ::toktok::Error<self::__lexer__::Token>> {
                        // Lex
                        let tokens = self::__lexer__::lex(source);

                        // Parse
                        let state = ::toktok::State::new(source, &tokens);
                        let (_, res) = self::__rules__::#name(state)?;

                        Ok(res)
                    }
                })
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    Ok(quote! { #(#parser)* })
}
