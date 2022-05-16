use crate::{
    ast,
    lexer::{lex, Token},
};
use toktok_core::{
    combinator::{
        alt, delimited, either, eoi, exact, many0, many1, opt, pair, sep0, sep1, sep_n, slice,
        terminated,
    },
    Parser,
};

type State<'s, 't> = toktok_core::State<'s, 't, Token>;
type PResult<'s, 't, O> = toktok_core::PResult<'s, 't, Token, O>;

pub fn parse(source: &str) -> Result<ast::Ast<'_>, toktok_core::Error<Token>> {
    // Lex
    let tokens = lex(source);

    // Parse
    let state = toktok_core::State::new(source, &tokens);
    let (_, ast) = ast(state)?;

    Ok(ast)
}

fn ast<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Ast<'s>>
where
    's: 't,
{
    let (state, rules) = many0(rule)(state)?;
    let (state, _) = eoi(state)?;

    Ok((state, ast::Ast { rules }))
}

fn rule<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Rule<'s>>
where
    's: 't,
{
    let (state, public) = opt(exact(Token::KeywordPublic))(state)?;
    let (state, name) = ident(state)?;
    let (state, _) = exact(Token::Colon)(state)?;
    let (state, ret_type) = ret_type(state)?;
    let (state, _) = exact(Token::Assign)(state)?;
    let (state, productions) = alt(
        delimited(
            exact(Token::LeftBrace),
            terminated(sep1(production, exact(Token::Comma)), opt(exact(Token::Comma))),
            exact(Token::RightBrace),
        ),
        production.map(|p| vec![p]),
    )(state)?;
    let (state, _) = exact(Token::Semicolon)(state)?;

    Ok((state, ast::Rule { is_public: public.is_some(), name, ret_type, productions }))
}

fn production<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Production<'s>>
where
    's: 't,
{
    let span_start = state.input().positioned_start();

    let (state, combinator) = combinator(state)?;
    let combinator = match combinator {
        ast::Combinator::Seq(seq) => seq,
        _ => ast::CombinatorSeq { source: combinator.source(), combinators: vec![combinator] },
    };

    // Arrow and rust expression are optional if seq has only one element
    let (state, is_fallible, rust_expression) = if combinator.combinators.len() == 1 {
        let (state, prod) = opt(pair(
            either(exact(Token::FatArrow), exact(Token::FatArrowFallible)).map(|e| e.is_right()),
            rust_expression,
        ))(state)?;
        match prod {
            Some((is_fallible, rust_expression)) => (state, is_fallible, rust_expression),
            None => (state, false, ast::RustExpression("{ $1.into() }")),
        }
    } else {
        let (state, (is_fallible, rust_expression)) = pair(
            either(exact(Token::FatArrow), exact(Token::FatArrowFallible)).map(|e| e.is_right()),
            rust_expression,
        )(state)?;
        (state, is_fallible, rust_expression)
    };

    let span_end = state.input().positioned_end(span_start);
    let source = &state.input().source()[span_start..span_end];

    Ok((state, ast::Production { combinator, rust_expression, is_fallible, source }))
}

fn combinator<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Combinator<'s>>
where
    's: 't,
{
    let (state, combinator) = alt(
        alt(
            combinator_seq.map(|c| ast::Combinator::Seq(c)),
            combinator_choice.map(|c| ast::Combinator::Choice(c)),
        ),
        combinator_leaf,
    )(state)?;

    Ok((state, combinator))
}

fn combinator_seq<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::CombinatorSeq<'s>>
where
    's: 't,
{
    let (state, (combinators, source)) =
        slice(sep_n(combinator_leaf, exact(Token::OperatorSeq), 2))(state)?;

    Ok((state, ast::CombinatorSeq { combinators, source }))
}

fn combinator_choice<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::CombinatorChoice<'s>>
where
    's: 't,
{
    let (state, (combinators, source)) =
        slice(sep_n(combinator_leaf, exact(Token::OperatorChoice), 2))(state)?;

    Ok((state, ast::CombinatorChoice { combinators, source }))
}

fn combinator_leaf<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Combinator<'s>>
where
    's: 't,
{
    let span_start = state.input().positioned_start();

    let (state, combinator) = alt(
        // Parenthesized
        delimited(exact(Token::LeftParen), combinator, exact(Token::RightParen)),
        // Atom
        slice(alt(
            alt(
                function_call.map(ast::CombinatorAtomKind::FunctionCall),
                token_short.map(ast::CombinatorAtomKind::TokenShort),
            ),
            path.map(ast::CombinatorAtomKind::Path),
        ))
        .map(|(kind, source)| ast::Combinator::Atom(ast::CombinatorAtom { kind, source })),
    )(state)?;
    let (state, suffix) = opt(alt(
        alt(exact(Token::QuestionMark).map(|_| '?'), exact(Token::OperatorMany0).map(|_| '*')),
        exact(Token::OperatorMany1).map(|_| '+'),
    ))(state)?;

    let span_end = state.input().positioned_end(span_start);
    let source = &state.input().source()[span_start..span_end];

    let combinator = match suffix {
        Some('?') => {
            ast::Combinator::Opt(ast::CombinatorOpt { combinator: Box::new(combinator), source })
        }
        Some('*') => ast::Combinator::Many0(ast::CombinatorMany0 {
            combinator: Box::new(combinator),
            source,
        }),
        Some('+') => ast::Combinator::Many1(ast::CombinatorMany1 {
            combinator: Box::new(combinator),
            source,
        }),
        Some(_) => unreachable!(),
        None => combinator,
    };

    Ok((state, combinator))
}

fn function_call<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::FunctionCall<'s>>
where
    's: 't,
{
    let (state, name) = path(state)?;
    let (state, args) = delimited(
        exact(Token::LeftParen),
        sep0(combinator, exact(Token::Comma)),
        exact(Token::RightParen),
    )(state)?;

    Ok((state, ast::FunctionCall { name, args }))
}

fn ret_type<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::RetType<'s>>
where
    's: 't,
{
    let (state, (_, ret_type)) = slice(many1(alt(
        alt(
            alt(
                alt(exact(Token::LeftAngle), exact(Token::RightAngle)).map(|_| ()),
                path.map(|_| ()),
            ),
            alt(exact(Token::SingleQuote), exact(Token::Comma)).map(|_| ()),
        ),
        alt(exact(Token::LeftParen), exact(Token::RightParen)).map(|_| ()),
    )))(state)?;

    Ok((state, ast::RetType(ret_type)))
}

fn rust_expression<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::RustExpression<'s>>
where
    's: 't,
{
    let (state, rust_expression) = exact(Token::RustExpression)(state)?;

    Ok((state, ast::RustExpression(rust_expression)))
}

fn token_short<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::TokenShort<'s>>
where
    's: 't,
{
    let (state, token_short) = exact(Token::TokenShort)(state)?;

    Ok((state, ast::TokenShort(token_short)))
}

fn path<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Path<'s>>
where
    's: 't,
{
    let (state, (_, path)) = slice(pair(ident, opt(pair(exact(Token::DoubleColon), path))))(state)?;

    Ok((state, ast::Path(path)))
}

fn ident<'s, 't>(state: State<'s, 't>) -> PResult<'s, 't, ast::Ident<'s>>
where
    's: 't,
{
    let (state, ident) = exact(Token::Identifier)(state)?;

    Ok((state, ast::Ident(ident)))
}
