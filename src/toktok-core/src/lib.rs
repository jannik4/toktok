#![deny(rust_2018_idioms)]

mod error;
mod span;
mod state;

pub mod combinator;

pub use self::{
    error::{Error, ErrorKind, ParserError, PrettyPrintOptions, StateError, TokenOrEoi},
    span::Span,
    state::{Input, SpannedToken, State},
};

pub type PResult<'s, 't, T, O> = Result<(State<'s, 't, T>, O), ParserError<T>>;

pub trait Parser<'s, 't, T, O> {
    fn parse(&self, state: State<'s, 't, T>) -> PResult<'s, 't, T, O>;

    fn as_ref(&self) -> ParserRef<'_, Self> {
        ParserRef { p: self }
    }

    fn map<O2, M>(self, m: M) -> Map<Self, M, O>
    where
        M: Fn(O) -> O2,
        Self: Sized,
    {
        Map { f: self, m, _p: std::marker::PhantomData }
    }
}

impl<'s, 't, T, O, F> Parser<'s, 't, T, O> for F
where
    T: 't,
    F: Fn(State<'s, 't, T>) -> PResult<'s, 't, T, O>,
{
    fn parse(&self, state: State<'s, 't, T>) -> PResult<'s, 't, T, O> {
        self(state)
    }
}

pub struct ParserRef<'p, P: ?Sized> {
    p: &'p P,
}

impl<'s, 't, T, O, P> Parser<'s, 't, T, O> for ParserRef<'_, P>
where
    P: Parser<'s, 't, T, O>,
{
    fn parse(&self, state: State<'s, 't, T>) -> PResult<'s, 't, T, O> {
        self.p.parse(state)
    }
}

pub struct Map<F, M, O> {
    f: F,
    m: M,
    _p: std::marker::PhantomData<*const O>,
}

impl<'s, 't, T, O, O2, F, M> Parser<'s, 't, T, O2> for Map<F, M, O>
where
    F: Parser<'s, 't, T, O>,
    M: Fn(O) -> O2,
{
    fn parse(&self, state: State<'s, 't, T>) -> PResult<'s, 't, T, O2> {
        let (state, output) = self.f.parse(state)?;
        Ok((state, (self.m)(output)))
    }
}
