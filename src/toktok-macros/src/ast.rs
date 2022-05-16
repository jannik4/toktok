#[derive(Debug)]
pub struct Ast<'a> {
    pub rules: Vec<Rule<'a>>,
}

#[derive(Debug)]
pub struct Rule<'a> {
    pub is_public: bool,
    pub name: Ident<'a>,
    pub ret_type: RetType<'a>,
    pub productions: Vec<Production<'a>>,
}

#[derive(Debug)]
pub struct Production<'a> {
    pub combinator: CombinatorSeq<'a>,
    pub rust_expression: RustExpression<'a>,
    pub is_fallible: bool,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum Combinator<'a> {
    Seq(CombinatorSeq<'a>),
    Choice(CombinatorChoice<'a>),
    Opt(CombinatorOpt<'a>),
    Many0(CombinatorMany0<'a>),
    Many1(CombinatorMany1<'a>),
    Atom(CombinatorAtom<'a>),
}

impl<'a> Combinator<'a> {
    pub fn source(&self) -> &'a str {
        match self {
            Combinator::Seq(c) => c.source,
            Combinator::Choice(c) => c.source,
            Combinator::Opt(c) => c.source,
            Combinator::Many0(c) => c.source,
            Combinator::Many1(c) => c.source,
            Combinator::Atom(c) => c.source,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CombinatorSeq<'a> {
    pub combinators: Vec<Combinator<'a>>,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct CombinatorChoice<'a> {
    pub combinators: Vec<Combinator<'a>>,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct CombinatorOpt<'a> {
    pub combinator: Box<Combinator<'a>>,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct CombinatorMany0<'a> {
    pub combinator: Box<Combinator<'a>>,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct CombinatorMany1<'a> {
    pub combinator: Box<Combinator<'a>>,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct CombinatorAtom<'a> {
    pub kind: CombinatorAtomKind<'a>,
    pub source: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum CombinatorAtomKind<'a> {
    Path(Path<'a>),
    TokenShort(TokenShort<'a>),
    FunctionCall(FunctionCall<'a>),
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall<'a> {
    pub name: Path<'a>,
    pub args: Vec<Combinator<'a>>,
}

#[derive(Debug)]
pub struct RetType<'a>(pub &'a str);

#[derive(Debug)]
pub struct RustExpression<'a>(pub &'a str);

#[derive(Debug, PartialEq)]
pub struct TokenShort<'a>(pub &'a str);

#[derive(Debug, PartialEq)]
pub struct Path<'a>(pub &'a str);

#[derive(Debug)]
pub struct Ident<'a>(pub &'a str);
