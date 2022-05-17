use crate::ast;
use anyhow::Result;
use proc_macro2::TokenStream;
use quote::quote;

pub fn generate(ast: &ast::Ast<'_>) -> Result<TokenStream> {
    let use_statements = ast
        .items
        .iter()
        .filter_map(|item| match item {
            ast::Item::UseStatement(use_statement) => {
                Some(use_statement.0.parse::<TokenStream>().expect("failed to parse use statement"))
            }
            ast::Item::Config(_) | ast::Item::Rule(_) => None,
        })
        .collect::<Vec<_>>();

    Ok(quote! { #(#use_statements)* })
}
