use logos::Logos;

#[derive(Logos, Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Token {
    #[token(";")]
    Semicolon,
    #[token("::")]
    DoubleColon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("<")]
    LeftAngle,
    #[token(">")]
    RightAngle,
    #[token("'")]
    SingleQuote,
    #[token("=")]
    Assign,
    #[token("?")]
    QuestionMark,

    #[token("pub")]
    KeywordPublic,

    #[token("=>")]
    FatArrow,
    #[token("=>?")]
    FatArrowFallible,

    #[token("~")]
    OperatorSeq,
    #[token("|")]
    OperatorChoice,
    // #[token("?")]
    // OperatorOpt,
    #[token("*")]
    OperatorMany0,
    #[token("+")]
    OperatorMany1,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex(r#""[^"]*""#)]
    TokenShort,

    RustExpression,

    #[error]
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[regex(r"//[^\r\n]*", logos::skip)]
    Error,
}

pub fn lex(source: &str) -> Vec<toktok_core::SpannedToken<Token>> {
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push(toktok_core::SpannedToken { token, span: lexer.span() });
        if token == Token::FatArrow || token == Token::FatArrowFallible {
            let span_start = lexer.span().end;
            parse_rust_expression(&mut lexer);
            let span = span_start..lexer.span().end;

            tokens.push(toktok_core::SpannedToken { token: Token::RustExpression, span });
        }
    }

    tokens
}

fn parse_rust_expression(lexer: &mut logos::Lexer<'_, Token>) {
    let mut balance = (0, 0, 0);
    let mut bump = 0;

    for c in lexer.remainder().chars() {
        match c {
            ',' | ';' | '}' if balance == (0, 0, 0) => break,

            '{' => balance.0 += 1,
            '}' => balance.0 -= 1,
            '[' => balance.1 += 1,
            ']' => balance.1 -= 1,
            '(' => balance.2 += 1,
            ')' => balance.2 -= 1,

            _ => (),
        }
        bump += c.len_utf8();
    }

    lexer.bump(bump);
}
