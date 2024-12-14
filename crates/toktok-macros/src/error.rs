use crate::lexer::Token;
use ariadne::{CharSet, Config, Label, Report, ReportKind, Source};
use proc_macro2::{LexError, TokenStream};
use quote::quote;
use std::fmt;
use toktok_core::{PrettyPrintOptions, TokenExpected};

#[derive(Debug)]
pub struct CompileError {
    message: String,
}

impl CompileError {
    pub fn from_message(message: impl Into<String>) -> Self {
        Self { message: message.into() }
    }

    pub fn from_toktok_error(error: toktok_core::Error<Token>, source: &str) -> Self {
        let range = error.span().as_range().cloned().unwrap_or(source.len()..source.len());
        let error = error.pretty_print(&PrettyPrintOptions {
            rename_token_expected: None,
            rename_token_found: None,
            filter_expected: Some(Box::new(|expected: &[TokenExpected<Token>]| {
                use std::collections::HashSet;

                // Filter duplicates
                let expected = expected.iter().copied().collect::<HashSet<_>>();

                // Sort
                let mut expected = expected.into_iter().collect::<Vec<_>>();
                expected.sort();

                expected
            })),
        });

        let mut buffer = Vec::new();
        Report::build(ReportKind::Error, range.clone())
            .with_message("Syntax Error")
            .with_label(Label::new(range).with_message(error))
            .with_config(Config::default().with_color(false).with_char_set(CharSet::Ascii))
            .finish()
            .write(Source::from(source), &mut buffer)
            .unwrap();

        Self { message: String::from_utf8(buffer).unwrap() }
    }
}

impl From<LexError> for CompileError {
    fn from(error: LexError) -> Self {
        Self::from_message(error.to_string())
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl quote::ToTokens for CompileError {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let message = self.to_string();
        tokens.extend(quote! {
            compile_error!(#message);
        });
    }
}
