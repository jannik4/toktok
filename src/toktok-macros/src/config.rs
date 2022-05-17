use crate::ast;

#[derive(Debug, Default)]
pub struct Config<'a> {
    pub lexer_skip: Vec<ast::Token<'a>>,
}

impl<'a> Config<'a> {
    pub fn build(ast: &ast::Ast<'a>) -> Self {
        let mut config = Config::default();

        for item in &ast.items {
            match item {
                ast::Item::Config(conf) => match conf.name.0 {
                    "lexer::skip" => match &conf.value {
                        ast::ConfigValue::Array(array) => {
                            config.lexer_skip = array
                                .iter()
                                .map(|value| match value {
                                    ast::ConfigValue::Array(_) => panic!("unexpected value"),
                                    ast::ConfigValue::Token(t) => *t,
                                })
                                .collect();
                        }
                        ast::ConfigValue::Token(t) => config.lexer_skip = vec![*t],
                    },
                    name => panic!("unknown config: {}", name),
                },
                ast::Item::UseStatement(_) | ast::Item::Rule(_) => (),
            }
        }

        config
    }
}
