use crate::lexer::Token;
use anyhow::{anyhow, Error};
use ariadne::{Label, Report, ReportKind, Source};
use toktok_core::{PrettyPrintOptions, TokenOrEoi};

pub fn pretty_print_toktok_error(error: toktok_core::Error<Token>, source: &str) -> Error {
    let range = error.span().as_range().cloned().unwrap_or(source.len()..source.len());
    let error = error.pretty_print(&PrettyPrintOptions {
        rename_token: None,
        filter_expected: Some(Box::new(|expected: &[TokenOrEoi<Token>]| {
            use std::collections::HashSet;

            // Filter duplicates
            let expected = expected.into_iter().copied().collect::<HashSet<_>>();

            // Sort
            let mut expected = expected.into_iter().collect::<Vec<_>>();
            expected.sort();

            expected
        })),
    });

    let mut buffer = Vec::new();
    Report::build(ReportKind::Error, (), range.start)
        .with_message("Syntax Error")
        .with_label(Label::new(range).with_message(error))
        .finish()
        .write(Source::from(source), &mut buffer)
        .unwrap();

    anyhow!(String::from_utf8(buffer).unwrap())
}
