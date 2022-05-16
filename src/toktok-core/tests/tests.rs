use toktok_core::{
    combinator::{alt, exact, pair},
    PResult, Parser, SpannedToken,
};

type State<'s, 't> = toktok_core::State<'s, 't, Token>;
// type Error = toktok_core::Error<Token>;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Token {
    Ident,
    Num,
    Semicolon,
}

// (id ~ num | id) ~ semi
fn statement<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, Token, ()>
where
    's: 't,
{
    let (state, _) = alt(
        pair(exact(Token::Ident), exact(Token::Num)).map(|_| ()),
        exact(Token::Ident).map(|_| ()),
    )(state)?;

    let (state, _) = exact(Token::Semicolon)(state)?;

    Ok((state, ()))
}

#[test]
fn test_ok() {
    let source = "abc 12;";
    let tokens = &[
        SpannedToken { token: Token::Ident, span: 0..3 },
        SpannedToken { token: Token::Num, span: 4..6 },
        SpannedToken { token: Token::Semicolon, span: 6..7 },
    ];
    let input = State::new(source, tokens);

    assert!(statement(input).is_ok());
}

#[test]
fn test_err() {
    let source = "abc def ghi";
    let tokens = &[
        SpannedToken { token: Token::Ident, span: 0..3 },
        SpannedToken { token: Token::Ident, span: 4..7 },
        SpannedToken { token: Token::Ident, span: 8..11 },
    ];
    let _input = State::new(source, tokens);

    let x = "abc";
    dbg!(&x[x.len()..x.len()]);
    //panic!("{:#?}", statement(input));
}

/*use toktok_core::{
    combinator::{alt, exact, seq},
    Parser, Result, SpannedToken,
};

type Input<'a> = toktok_core::Input<'a, Token>;
type ErrorState = toktok_core::ErrorState<Token>;
// type Error = toktok_core::Error<Token>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Ident,
    Num,
    Semicolon,
}

// (id ~ num | id) ~ semi
fn statement(input: Input<'_>, error_state: ErrorState) -> Result<'_, Token, ()> {
    let (input, error_state, _) = alt(
        seq(exact(Token::Ident), exact(Token::Num)).map(|_| ()),
        exact(Token::Ident).map(|_| ()),
    )(input, error_state)?;

    let (input, error_state, _) = exact(Token::Semicolon)(input, error_state)?;

    Ok((input, error_state, ()))
}

#[test]
fn test_ok() {
    let source = "abc 12;";
    let tokens = &[
        SpannedToken { token: Token::Ident, span: 0..3 },
        SpannedToken { token: Token::Num, span: 4..6 },
        SpannedToken { token: Token::Semicolon, span: 6..7 },
    ];
    let input = Input::new(source, tokens);

    assert!(statement(input, ErrorState::ok()).is_ok());
}

#[test]
fn test_err() {
    let source = "abc def ghi";
    let tokens = &[
        SpannedToken { token: Token::Ident, span: 0..3 },
        SpannedToken { token: Token::Ident, span: 4..7 },
        SpannedToken { token: Token::Ident, span: 8..11 },
    ];
    let input = Input::new(source, tokens);

    panic!("{:#?}", statement(input, ErrorState::ok()));
}*/
