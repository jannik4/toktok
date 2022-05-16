use logos::Logos;
use serde_json::Value;
use toktok_macros::make_parser;

make_parser!(grammar = "tests/json.toktok");

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Token {
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex(r"[+-]?[0-9_]+")]
    Integer,
    #[regex(r###""(?:[^"\\]|\\.)*""###)]
    String,

    #[error]
    #[regex(r"[ \t\r\n]+", logos::skip)]
    #[regex(r"#[^\r\n]*", logos::skip)]
    Error,
}

pub fn parse(source: &str) -> Result<Value, toktok::Error<Token>> {
    // Lex
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next() {
        tokens.push(toktok::SpannedToken { token, span: lexer.span() });
    }

    // Parse
    let state = toktok::State::new(source, &tokens);
    let (_, json) = parser::json(state)?;

    Ok(json)
}

#[test]
fn json() {
    let source = r###"
    {
        "hello": "world",
        "x": [true, 12, false, -1, "true"],
        "y": { "zzz": "", "a": [] }
    }
    "###;
    let res = parse(source).unwrap();

    let expected = serde_json::from_str::<Value>(source).unwrap();
    assert_eq!(res, expected);
}
