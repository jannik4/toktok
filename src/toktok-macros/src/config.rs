use crate::{
    ast::{Ast, ConfigValue, Item, Token, TokenRegex},
    CompileError, Result,
};

#[derive(Debug)]
pub struct Config<'a> {
    pub lexer_skip: Vec<TokenRegex<'a>>,
}

impl<'a> Config<'a> {
    pub fn build(ast: &Ast<'a>) -> Result<Self> {
        let mut lexer_skip = None;

        for item in &ast.items {
            match item {
                Item::Config(conf) => match conf.name.0 {
                    "lexer::skip" => {
                        let new_value = match &conf.value {
                            ConfigValue::Array(array) => array
                                .iter()
                                .map(|value| match value {
                                    ConfigValue::Array(_) => {
                                        Err(CompileError::from_message("unexpected value"))
                                    }
                                    ConfigValue::Token(Token::TokenLit(_)) => {
                                        Err(CompileError::from_message("unexpected value"))
                                    }
                                    ConfigValue::Token(Token::TokenRegex(t)) => Ok(*t),
                                })
                                .collect::<Result<_>>()?,
                            ConfigValue::Token(Token::TokenLit(_)) => {
                                return Err(CompileError::from_message("unexpected value"))
                            }
                            ConfigValue::Token(Token::TokenRegex(t)) => vec![*t],
                        };

                        assert!(lexer_skip.is_none(), "duplicate config: {}", conf.name.0);
                        lexer_skip = Some(new_value);
                    }
                    name => {
                        return Err(CompileError::from_message(format!("unknown config: {}", name)))
                    }
                },
                Item::UseStatement(_) | Item::Rule(_) => (),
            }
        }

        Ok(Config { lexer_skip: lexer_skip.unwrap_or_default() })
    }
}
