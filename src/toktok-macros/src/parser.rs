use crate::{
    ast,
    lexer::{lex, Token},
    CompileError,
};
use toktok_core::{
    combinator::{
        alt, delimited, either, eoi, exact, many0, many1, opt, sep0, sep1, sep_n, seq, slice,
        terminated,
    },
    Parser,
};

type State<'s, 't> = toktok_core::State<'s, 't, Token>;
type Result<'s, 't, O> = toktok_core::Result<'s, 't, Token, O>;

pub fn parse(source: &str) -> crate::Result<ast::Ast<'_>> {
    // Lex
    let tokens = lex(source);

    // Parse
    let state = toktok_core::State::new(source, &tokens);
    let (_, ast) = ast(state).map_err(|e| CompileError::from_toktok_error(e.into(), source))?;

    Ok(ast)
}

fn ast<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Ast<'s>>
where
    's: 't,
{
    let (state, items) = many0(item)(state)?;
    let (state, _) = eoi(state)?;

    Ok((state, ast::Ast { items }))
}

fn item<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Item<'s>>
where
    's: 't,
{
    alt((
        use_statement.map(ast::Item::UseStatement),
        config.map(ast::Item::Config),
        rule.map(ast::Item::Rule),
    ))(state)
}

fn use_statement<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::UseStatement<'s>>
where
    's: 't,
{
    let (state, (_, use_statement)) = slice(seq((
        opt(exact(Token::KeywordPublic)),
        exact(Token::KeywordUse),
        many1(alt((
            exact(Token::LeftBrace).map(|_| ()),
            exact(Token::RightBrace).map(|_| ()),
            exact(Token::DoubleColon).map(|_| ()),
            exact(Token::Comma).map(|_| ()),
            exact(Token::OperatorMany0).map(|_| ()),
            ident.map(|_| ()),
        ))),
        exact(Token::Semicolon),
    )))(state)?;

    Ok((state, ast::UseStatement(use_statement)))
}

fn config<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Config<'s>>
where
    's: 't,
{
    let (state, _) = exact(Token::At)(state)?;
    let (state, name) = path(state)?;
    let (state, _) = exact(Token::Assign)(state)?;
    let (state, value) = config_value(state)?;
    let (state, _) = exact(Token::Semicolon)(state)?;

    Ok((state, ast::Config { name, value }))
}

fn config_value<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::ConfigValue<'s>>
where
    's: 't,
{
    alt((
        delimited(
            exact(Token::LeftBracket),
            sep0(config_value, exact(Token::Comma)),
            exact(Token::RightBracket),
        )
        .map(ast::ConfigValue::Array),
        token.map(ast::ConfigValue::Token),
    ))(state)
}

fn rule<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Rule<'s>>
where
    's: 't,
{
    let (state, public) = opt(exact(Token::KeywordPublic))(state)?;
    let (state, name) = ident(state)?;
    let (state, _) = exact(Token::Colon)(state)?;
    let (state, ret_type) = ret_type(state)?;
    let (state, _) = exact(Token::Assign)(state)?;
    let (state, productions) = alt((
        delimited(
            exact(Token::LeftBrace),
            terminated(sep1(production, exact(Token::Comma)), opt(exact(Token::Comma))),
            exact(Token::RightBrace),
        ),
        production.map(|p| vec![p]),
    ))(state)?;
    let (state, _) = exact(Token::Semicolon)(state)?;

    Ok((state, ast::Rule { is_public: public.is_some(), name, ret_type, productions }))
}

fn production<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Production<'s>>
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
        let (state, prod) = opt(seq((
            either(exact(Token::FatArrow), exact(Token::FatArrowFallible)).map(|e| e.is_right()),
            rust_expression,
        )))(state)?;
        match prod {
            Some((is_fallible, rust_expression)) => (state, is_fallible, rust_expression),
            None => (state, false, ast::RustExpression("{ $1.into() }")),
        }
    } else {
        let (state, (is_fallible, rust_expression)) = seq((
            either(exact(Token::FatArrow), exact(Token::FatArrowFallible)).map(|e| e.is_right()),
            rust_expression,
        ))(state)?;
        (state, is_fallible, rust_expression)
    };

    let span_end = state.input().positioned_end(span_start);
    let source = &state.input().source()[span_start..span_end];

    Ok((state, ast::Production { combinator, rust_expression, is_fallible, source }))
}

fn combinator<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Combinator<'s>>
where
    's: 't,
{
    let (state, combinator) = alt((
        combinator_seq.map(ast::Combinator::Seq),
        combinator_choice.map(ast::Combinator::Choice),
        combinator_leaf,
    ))(state)?;

    Ok((state, combinator))
}

fn combinator_seq<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::CombinatorSeq<'s>>
where
    's: 't,
{
    let (state, (combinators, source)) =
        slice(sep_n(combinator_leaf, exact(Token::OperatorSeq), 2))(state)?;

    Ok((state, ast::CombinatorSeq { combinators, source }))
}

fn combinator_choice<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::CombinatorChoice<'s>>
where
    's: 't,
{
    let (state, (combinators, source)) =
        slice(sep_n(combinator_leaf, exact(Token::OperatorChoice), 2))(state)?;

    Ok((state, ast::CombinatorChoice { combinators, source }))
}

fn combinator_leaf<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Combinator<'s>>
where
    's: 't,
{
    let span_start = state.input().positioned_start();

    let (state, combinator) = alt((
        // Parenthesized
        delimited(exact(Token::LeftParen), combinator, exact(Token::RightParen)),
        // Atom
        slice(alt((
            token.map(ast::CombinatorAtomKind::Token),
            function_call.map(ast::CombinatorAtomKind::FunctionCall),
            path.map(ast::CombinatorAtomKind::Path),
        )))
        .map(|(kind, source)| ast::Combinator::Atom(ast::CombinatorAtom { kind, source })),
    ))(state)?;
    let (state, suffix) = opt(alt((
        exact(Token::QuestionMark).map(|_| '?'),
        exact(Token::OperatorMany0).map(|_| '*'),
        exact(Token::OperatorMany1).map(|_| '+'),
    )))(state)?;

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

fn function_call<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::FunctionCall<'s>>
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

fn ret_type<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::RetType<'s>>
where
    's: 't,
{
    let (state, (_, ret_type)) = slice(many1(alt((
        exact(Token::LeftAngle).map(|_| ()),
        exact(Token::RightAngle).map(|_| ()),
        exact(Token::SingleQuote).map(|_| ()),
        exact(Token::Comma).map(|_| ()),
        exact(Token::LeftParen).map(|_| ()),
        exact(Token::RightParen).map(|_| ()),
        path.map(|_| ()),
    ))))(state)?;

    Ok((state, ast::RetType(ret_type)))
}

fn rust_expression<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::RustExpression<'s>>
where
    's: 't,
{
    let (state, rust_expression) = exact(Token::RustExpression)(state)?;

    Ok((state, ast::RustExpression(rust_expression)))
}

fn token<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Token<'s>>
where
    's: 't,
{
    let (state, token) =
        alt((token_lit.map(ast::Token::TokenLit), token_regex.map(ast::Token::TokenRegex)))(state)?;

    Ok((state, token))
}

fn token_lit<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::TokenLit<'s>>
where
    's: 't,
{
    let (state, token_lit) = exact(Token::TokenLit)(state)?;

    Ok((state, ast::TokenLit(&token_lit[1..token_lit.len() - 1])))
}

fn token_regex<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::TokenRegex<'s>>
where
    's: 't,
{
    let (state, token_regex) = exact(Token::TokenRegex)(state)?;

    Ok((state, ast::TokenRegex(&token_regex[2..token_regex.len() - 1])))
}

fn path<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Path<'s>>
where
    's: 't,
{
    let (state, (_, path)) =
        slice(seq((ident, opt(seq((exact(Token::DoubleColon), path))))))(state)?;

    Ok((state, ast::Path(path)))
}

fn ident<'s, 't>(state: State<'s, 't>) -> Result<'s, 't, ast::Ident<'s>>
where
    's: 't,
{
    let (state, ident) = exact(Token::Identifier)(state)?;

    Ok((state, ast::Ident(ident)))
}
