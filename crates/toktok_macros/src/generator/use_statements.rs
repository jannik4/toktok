use crate::{CompileError, ast};
use proc_macro2::TokenStream;
use quote::quote;

pub fn generate(ast: &ast::Ast<'_>, error_sink: &mut impl FnMut(CompileError)) -> TokenStream {
    let use_statements = ast
        .items
        .iter()
        .filter_map(|item| match item {
            ast::Item::UseStatement(use_statement) => {
                Some(match use_statement.0.parse::<TokenStream>() {
                    Ok(tokens) => Some(tokens),
                    Err(error) => {
                        error_sink(error.into());
                        None
                    }
                })
            }
            ast::Item::Config(_) | ast::Item::Rule(_) => None,
        })
        .collect::<Vec<_>>();

    quote! { #(#use_statements)* }
}
