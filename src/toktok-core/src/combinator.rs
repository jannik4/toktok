use crate::{Error, PResult, Parser, ParserError, Span, State, StateError, TokenOrEoi};
use either::Either;
use std::ops::Range;

pub fn eoi<'s, 't, T>(state: State<'s, 't, T>) -> PResult<'s, 't, T, ()>
where
    T: Clone,
{
    if state.input().is_empty() {
        return Ok((state, ()));
    }

    if state.curr_err().is_some() {
        return Err(state.into_parts().1.unwrap());
    }

    let found = state.first().unwrap().token.clone();
    Err(state.and_error(Error::new_expected(
        Span::Eoi,
        vec![TokenOrEoi::Eoi],
        TokenOrEoi::Token(found),
    )))
}

pub fn peek<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    move |state| {
        let (input, curr_err) = state.into_parts();
        match f.parse(State::from_parts(input, StateError::none())) {
            Ok((_rest, out)) => Ok((State::from_parts(input, curr_err), out)),
            Err(e) => Err(curr_err.and(e.into())),
        }
    }
}

pub fn peek_negative<'s, 't, T, O, F>(
    f: F,
    err: impl std::error::Error + Send + Sync + Clone + 'static,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, ()>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    move |state: State<'s, 't, T>| {
        let (input, curr_err) = state.into_parts();
        match positioned(f.as_ref()).parse(State::from_parts(input, StateError::none())) {
            Ok((_rest, (_out, range))) => {
                Err(curr_err.and(Error::new_expected_negative(range, Box::new(err.clone()))))
            }
            Err(e) => Ok((e.inore_fail().recover(input)?, ())),
        }
    }
}

pub fn fail<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    move |state| Ok(f.parse(state).map_err(ParserError::with_is_fail)?)
}

pub fn box_<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Box<O>>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    let boxed_f = f.map(Box::new);

    move |state| Ok(boxed_f.parse(state)?)
}

pub fn opt<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Option<O>>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    move |state| {
        let input = state.input();
        match f.parse(state) {
            Ok((rest, out)) => Ok((rest, Some(out))),
            Err(e) => Ok((e.recover(input)?, None)),
        }
    }
}

pub fn many0<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Vec<O>>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    many_n(f, 0)
}

pub fn many1<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Vec<O>>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    many_n(f, 1)
}

pub fn many_n<'s, 't, T, O, F>(
    f: F,
    n: usize,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Vec<O>>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    move |mut state| {
        let mut acc = Vec::new();

        loop {
            let input = state.input();
            match f.parse(state) {
                Ok((rest, output)) => {
                    acc.push(output);
                    state = rest;
                }
                Err(e) => {
                    if acc.len() < n {
                        return Err(e);
                    }

                    state = e.recover(input)?;
                    break;
                }
            }
        }

        Ok((state, acc))
    }
}

pub fn sep0<'s, 't, T, O, O2, F, S>(
    f: F,
    sep: S,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Vec<O>>
where
    F: Parser<'s, 't, T, O>,
    S: Parser<'s, 't, T, O2>,
    's: 't,
{
    move |state| {
        let mut acc = Vec::new();

        let mut state = {
            let input = state.input();
            match f.parse(state) {
                Ok((rest, out)) => {
                    acc.push(out);
                    rest
                }
                Err(e) => return Ok((e.recover(input)?, acc)),
            }
        };

        loop {
            let input = state.input();
            match pair(sep.as_ref(), f.as_ref()).parse(state) {
                Ok((rest, (_, output))) => {
                    acc.push(output);
                    state = rest;
                }
                Err(e) => {
                    state = e.recover(input)?;
                    break;
                }
            };
        }

        Ok((state, acc))
    }
}

pub fn sep1<'s, 't, T, O, O2, F, S>(
    f: F,
    sep: S,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Vec<O>>
where
    F: Parser<'s, 't, T, O>,
    S: Parser<'s, 't, T, O2>,
    's: 't,
{
    sep_n(f, sep, 1)
}

pub fn sep_n<'s, 't, T, O, O2, F, S>(
    f: F,
    sep: S,
    n: usize,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Vec<O>>
where
    F: Parser<'s, 't, T, O>,
    S: Parser<'s, 't, T, O2>,
    's: 't,
{
    assert!(n >= 1);

    move |mut state| {
        let mut acc = Vec::new();

        let (rest, output) = f.parse(state)?;
        acc.push(output);
        state = rest;

        loop {
            let input = state.input();
            match pair(sep.as_ref(), f.as_ref()).parse(state) {
                Ok((rest, (_, output))) => {
                    acc.push(output);
                    state = rest;
                }
                Err(e) => {
                    if acc.len() < n {
                        return Err(e);
                    }

                    state = e.recover(input)?;
                    break;
                }
            };
        }

        Ok((state, acc))
    }
}

pub fn pair<'s, 't, T, O1, O2, F1, F2>(
    f1: F1,
    f2: F2,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, (O1, O2)>
where
    F1: Parser<'s, 't, T, O1>,
    F2: Parser<'s, 't, T, O2>,
    's: 't,
{
    move |state| {
        let (state, out1) = f1.parse(state)?;
        let (state, out2) = f2.parse(state)?;

        Ok((state, (out1, out2)))
    }
}

pub fn preceded<'s, 't, T, O1, O2, F1, F2>(
    f1: F1,
    f2: F2,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O2>
where
    F1: Parser<'s, 't, T, O1>,
    F2: Parser<'s, 't, T, O2>,
    's: 't,
{
    move |state| {
        let (state, _out1) = f1.parse(state)?;
        let (state, out2) = f2.parse(state)?;

        Ok((state, out2))
    }
}

pub fn terminated<'s, 't, T, O1, O2, F1, F2>(
    f1: F1,
    f2: F2,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O1>
where
    F1: Parser<'s, 't, T, O1>,
    F2: Parser<'s, 't, T, O2>,
    's: 't,
{
    move |state| {
        let (state, out1) = f1.parse(state)?;
        let (state, _out2) = f2.parse(state)?;

        Ok((state, out1))
    }
}

pub fn delimited<'s, 't, T, O1, O2, O3, F1, F2, F3>(
    f1: F1,
    f2: F2,
    f3: F3,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O2>
where
    F1: Parser<'s, 't, T, O1>,
    F2: Parser<'s, 't, T, O2>,
    F3: Parser<'s, 't, T, O3>,
    's: 't,
{
    move |state| {
        let (state, _out1) = f1.parse(state)?;
        let (state, out2) = f2.parse(state)?;
        let (state, _out3) = f3.parse(state)?;

        Ok((state, out2))
    }
}

pub fn alt<'s, 't, T, O, F1, F2>(
    f1: F1,
    f2: F2,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O>
where
    F1: Parser<'s, 't, T, O>,
    F2: Parser<'s, 't, T, O>,
    's: 't,
{
    move |state| {
        let input = state.input();
        let state = match f1.parse(state) {
            Ok((rest, out)) => return Ok((rest, out)),
            Err(e) => e.recover(input)?,
        };

        match f2.parse(state) {
            Ok((rest, out)) => Ok((rest, out)),
            Err(e) => Err(e.into()),
        }
    }
}

pub fn either<'s, 't, T, O1, O2, F1, F2>(
    f1: F1,
    f2: F2,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, Either<O1, O2>>
where
    F1: Parser<'s, 't, T, O1>,
    F2: Parser<'s, 't, T, O2>,
    's: 't,
{
    move |state| {
        let input = state.input();
        let state = match f1.parse(state) {
            Ok((rest, out)) => return Ok((rest, Either::Left(out))),
            Err(e) => e.recover(input)?,
        };

        match f2.parse(state) {
            Ok((rest, out)) => Ok((rest, Either::Right(out))),
            Err(e) => Err(e.into()),
        }
    }
}

pub fn exact<'s, 't, T>(token: T) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, &'s str>
where
    T: Clone + PartialEq,
    's: 't,
{
    move |state: State<'s, 't, T>| match state.first() {
        Some(spanned) if spanned.token == token => Ok(state.split_first()),
        Some(spanned) => Err(state.and_error(Error::new_expected(
            Span::Range(spanned.span.clone()),
            vec![TokenOrEoi::Token(token.clone())],
            TokenOrEoi::Token(spanned.token.clone()),
        ))),
        None => Err(state.and_error(Error::new_expected(
            Span::Eoi,
            vec![TokenOrEoi::Token(token.clone())],
            TokenOrEoi::Eoi,
        ))),
    }
}

pub fn positioned<'s, 't, T, O, F>(
    f: F,
) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, (O, Range<usize>)>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    move |state| {
        let start = state.input().positioned_start();
        let (state, output) = f.parse(state)?;
        let end = state.input().positioned_end(start);

        Ok((state, (output, start..end)))
    }
}

pub fn slice<'s, 't, T, O, F>(f: F) -> impl Fn(State<'s, 't, T>) -> PResult<'s, 't, T, (O, &'s str)>
where
    F: Parser<'s, 't, T, O>,
    's: 't,
{
    let positioned_f = positioned(f);

    move |state| {
        let (state, (output, range)) = positioned_f.parse(state)?;
        let slice = &state.input().source()[range];

        Ok((state, (output, slice)))
    }
}
