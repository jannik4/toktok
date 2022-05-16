use crate::{Error, ParserError, StateError};
use std::ops::Range;

#[derive(Debug)]
pub struct State<'s, 't, T> {
    input: Input<'s, 't, T>,
    err: StateError<T>,
}

impl<'s, 't, T> State<'s, 't, T> {
    pub fn new(source: &'s str, tokens: &'t [SpannedToken<T>]) -> Self {
        Self { input: Input::new(source, tokens), err: StateError::none() }
    }

    pub fn from_parts(input: Input<'s, 't, T>, err: StateError<T>) -> Self {
        Self { input, err }
    }

    pub fn into_parts(self) -> (Input<'s, 't, T>, StateError<T>) {
        (self.input, self.err)
    }

    pub fn input(&self) -> Input<'s, 't, T> {
        self.input
    }

    pub fn first(&self) -> Option<&'t SpannedToken<T>> {
        self.input.tokens.get(0)
    }

    pub fn split_first(self) -> (Self, &'s str) {
        let (first, tokens) = self.input.tokens.split_first().expect("expected at least one token");

        (
            Self {
                input: Input {
                    source: self.input.source,
                    tokens,
                    last_token_position_end: first.span.end,
                },
                err: self.err,
            },
            &self.input.source[first.span.clone()],
        )
    }

    pub fn curr_err(&self) -> &StateError<T> {
        &self.err
    }

    pub fn and_error(self, err: Error<T>) -> ParserError<T> {
        self.err.and(err)
    }
}

#[derive(Debug)]
pub struct Input<'s, 't, T> {
    source: &'s str,
    tokens: &'t [SpannedToken<T>],
    last_token_position_end: usize,
}

impl<'s, 't, T> Input<'s, 't, T> {
    fn new(source: &'s str, tokens: &'t [SpannedToken<T>]) -> Self {
        Input {
            source,
            tokens,
            last_token_position_end: match tokens.get(0) {
                Some(token) => token.span.start,
                None => 0,
            },
        }
    }

    pub fn source(&self) -> &'s str {
        self.source
    }

    pub fn positioned_start(&self) -> usize {
        match self.tokens.get(0) {
            Some(token) => token.span.start,
            None => self.source.len(),
        }
    }

    pub fn positioned_end(&self, start: usize) -> usize {
        // If no tokens were consumed since start, last_token_position_end is still at
        // the end of the last token before start. Therefore set end to the maximum
        // of start and end.
        std::cmp::max(start, self.last_token_position_end)
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}

impl<T> Clone for Input<'_, '_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Input<'_, '_, T> {}

#[derive(Debug)]
pub struct SpannedToken<T> {
    pub token: T,
    pub span: Range<usize>,
}
