use crate::{
    ast::{Ast, ConfigValue, Item, Token, TokenRegex},
    CompileError,
};

#[derive(Debug)]
pub struct Config<'a> {
    pub lexer_skip: Vec<TokenRegex<'a>>,
}

impl<'a> Config<'a> {
    pub fn build(ast: &Ast<'a>, error_sink: &mut impl FnMut(CompileError)) -> Self {
        let mut lexer_skip = None;

        for item in &ast.items {
            match item {
                Item::Config(conf) => match conf.name.0 {
                    "lexer::skip" => {
                        let new_value = match &conf.value {
                            ConfigValue::Array(array) => array
                                .iter()
                                .filter_map(|value| match value {
                                    ConfigValue::Array(_) => {
                                        error_sink(CompileError::from_message("unexpected value"));
                                        None
                                    }
                                    ConfigValue::Token(Token::TokenLit(_)) => {
                                        error_sink(CompileError::from_message("unexpected value"));
                                        None
                                    }
                                    ConfigValue::Token(Token::TokenRegex(t)) => Some(*t),
                                })
                                .collect(),
                            ConfigValue::Token(Token::TokenLit(_)) => {
                                error_sink(CompileError::from_message("unexpected value"));
                                continue;
                            }
                            ConfigValue::Token(Token::TokenRegex(t)) => vec![*t],
                        };

                        assert!(lexer_skip.is_none(), "duplicate config: {}", conf.name.0);
                        lexer_skip = Some(new_value);
                    }
                    name => {
                        error_sink(CompileError::from_message(format!("unknown config: {}", name)));
                    }
                },
                Item::UseStatement(_) | Item::Rule(_) => (),
            }
        }

        Config { lexer_skip: lexer_skip.unwrap_or_default() }
    }
}
