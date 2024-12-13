use crate::{ast, Result};
use proc_macro2::TokenStream;
use quote::quote;

pub fn generate(ast: &ast::Ast<'_>) -> Result<TokenStream> {
    let use_statements = ast
        .items
        .iter()
        .filter_map(|item| match item {
            ast::Item::UseStatement(use_statement) => {
                Some(match use_statement.0.parse::<TokenStream>() {
                    Ok(tokens) => Ok(tokens),
                    Err(error) => Err(error.into()),
                })
            }
            ast::Item::Config(_) | ast::Item::Rule(_) => None,
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(quote! { #(#use_statements)* })
}
