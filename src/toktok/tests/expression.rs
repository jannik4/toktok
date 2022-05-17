use toktok_macros::make_parser;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    UnaryTerm(UnaryOperator, Box<Expression>),
    BinaryTerm(Box<Expression>, BinaryOperator, Box<Expression>),
    Number(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Sign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

fn expr_foldl(
    lhs: Expression,
    rest: impl IntoIterator<Item = (BinaryOperator, Expression)>,
) -> Expression {
    rest.into_iter().fold(lhs, |lhs, (operator, rhs)| {
        Expression::BinaryTerm(Box::new(lhs), operator, Box::new(rhs))
    })
}

make_parser!(grammar = "tests/expression.toktok");

#[test]
fn simple() {
    let source = "17 + 3";
    let res = parser::expression(source).unwrap();

    let expected = Expression::BinaryTerm(
        Box::new(Expression::Number(17)),
        BinaryOperator::Add,
        Box::new(Expression::Number(3)),
    );
    assert_eq!(res, expected);
}

#[test]
fn associativity() {
    let source = "4 - 3 - 2";
    let res = parser::expression(source).unwrap();

    let expected = Expression::BinaryTerm(
        Box::new(Expression::BinaryTerm(
            Box::new(Expression::Number(4)),
            BinaryOperator::Sub,
            Box::new(Expression::Number(3)),
        )),
        BinaryOperator::Sub,
        Box::new(Expression::Number(2)),
    );
    assert_eq!(res, expected);
}

#[test]
fn misc() {
    let source = "(1 + 2) - 3 / -4";
    let res = parser::expression(source).unwrap();

    let expected = Expression::BinaryTerm(
        Box::new(Expression::BinaryTerm(
            Box::new(Expression::Number(1)),
            BinaryOperator::Add,
            Box::new(Expression::Number(2)),
        )),
        BinaryOperator::Sub,
        Box::new(Expression::BinaryTerm(
            Box::new(Expression::Number(3)),
            BinaryOperator::Div,
            Box::new(Expression::UnaryTerm(UnaryOperator::Sign, Box::new(Expression::Number(4)))),
        )),
    );
    assert_eq!(res, expected);
}
