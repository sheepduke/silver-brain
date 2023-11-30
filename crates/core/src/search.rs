use std::error::Error;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_until, take_while, take_while1},
    character::{
        complete::{char, space0},
        streaming::space1,
    },
    combinator::{fail, into, success, value},
    error::context,
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

// ============================================================
//  Query
// ============================================================

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum CompareOperator {
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
}

#[derive(Clone, Debug)]
pub enum Query {
    Keyword(String),
    Filter {
        key: String,
        op: CompareOperator,
        value: String,
    },
    Property {
        key: String,
        op: CompareOperator,
        value: String,
    },
    And(Vec<Query>),
    Or(Vec<Query>),
    Not(Box<Query>),
}

pub struct InvalidSearchError;

impl<T: Error> From<T> for InvalidSearchError {
    fn from(_: T) -> Self {
        Self
    }
}

pub fn parse(input: &str) -> Result<Query, InvalidSearchError> {
    let (remaining, expr) = parse_expr(input)?;

    if !remaining.is_empty() {
        Err(InvalidSearchError)
    } else {
        Ok(expr_to_query(&expr))
    }
}

fn expr_to_query(expr: &Expr) -> Query {
    todo!()
}

// ============================================================
//  Expr
// ============================================================

#[derive(Clone, Debug)]
enum Expr {
    String(String),
    Not,
    And,
    Or,
    Filter {
        key: String,
        op: CompareOperator,
        value: String,
    },
    Property {
        key: String,
        op: CompareOperator,
        value: String,
    },
    Sequence(Vec<Expr>),
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, output) = many0(alt((parse_paren, parse_seq)))(input)?;

    Ok((remaining, Expr::Sequence(output)))
}

fn parse_paren(input: &str) -> IResult<&str, Expr> {
    delimited(
        terminated(char('('), space0),
        parse_expr,
        terminated(char(')'), space0),
    )(input)
}

fn parse_seq(input: &str) -> IResult<&str, Expr> {
    let (remaining, output) = many1(parse_single)(input)?;

    Ok((remaining, Expr::Sequence(output)))
}

fn parse_single(input: &str) -> IResult<&str, Expr> {
    alt((
        parse_not_expr,
        parse_and_expr,
        parse_or_expr,
        parse_filter_expr,
        parse_property_expr,
        parse_string_expr,
    ))(input)
}

fn parse_not_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, _) = alt((
        terminated(tag("!"), space0),
        terminated(tag_no_case("NOT"), space1),
    ))(input)?;

    Ok((remaining, Expr::Not))
}

fn parse_and_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, _) = alt((
        terminated(tag("&&"), space0),
        terminated(tag_no_case("AND"), space1),
    ))(input)?;

    Ok((remaining, Expr::And))
}

fn parse_or_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, _) = alt((
        terminated(tag("||"), space0),
        terminated(tag_no_case("OR"), space1),
    ))(input)?;

    Ok((remaining, Expr::Or))
}

fn parse_filter_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, (key, op, value)) =
        tuple((parse_any_string, parse_filter_op, parse_any_string))(input)?;

    Ok((remaining, Expr::Filter { key, op, value }))
}

fn parse_filter_op(input: &str) -> IResult<&str, CompareOperator> {
    terminated(
        alt((
            value(CompareOperator::Equal, tag(":")),
            value(CompareOperator::NotEqual, tag(":!")),
            value(CompareOperator::Equal, tag(":=")),
            value(CompareOperator::NotEqual, tag(":!=")),
            value(CompareOperator::Less, tag(":<")),
            value(CompareOperator::LessEqual, tag(":<=")),
            value(CompareOperator::Greater, tag(":>")),
            value(CompareOperator::GreaterEqual, tag(":>=")),
        )),
        space0,
    )(input)
}

fn parse_property_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, (key, op, value)) =
        tuple((parse_any_string, parse_property_op, parse_any_string))(input)?;

    Ok((remaining, Expr::Property { key, op, value }))
}

fn parse_property_op(input: &str) -> IResult<&str, CompareOperator> {
    terminated(
        alt((
            value(CompareOperator::Equal, tag("=")),
            value(CompareOperator::NotEqual, tag("!=")),
            value(CompareOperator::Less, tag("<")),
            value(CompareOperator::LessEqual, tag("<=")),
            value(CompareOperator::Greater, tag(">")),
            value(CompareOperator::GreaterEqual, tag(">=")),
        )),
        space0,
    )(input)
}

fn parse_string_expr(input: &str) -> IResult<&str, Expr> {
    let (remaining, output) = parse_any_string(input)?;

    Ok((remaining, Expr::String(output)))
}

fn parse_any_string(input: &str) -> IResult<&str, String> {
    terminated(alt((parse_quoted_string, parse_basic_string)), space0)(input)
}

fn parse_quoted_string(input: &str) -> IResult<&str, String> {
    let (remaining, output) = delimited(char('"'), take_until("\""), char('"'))(input)?;

    Ok((remaining, output.to_string()))
}

fn parse_basic_string(input: &str) -> IResult<&str, String> {
    let (remaining, output) = take_while1(is_basic_char)(input)?;

    if output == "&&" || output == "||" || output == "AND" || output == "OR" || output == "NOT" {
        fail(input)
    } else {
        Ok((remaining, output.to_string()))
    }
}

fn is_basic_char(ch: char) -> bool {
    ch != ' '
        && ch != '"'
        && ch != ':'
        && ch != '='
        && ch != '<'
        && ch != '>'
        && ch != '!'
        && ch != '('
        && ch != ')'
}
