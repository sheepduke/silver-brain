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

#[derive(Debug)]
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
        Ok(expr.try_into()?)
    }
}

enum ParseState {
    Normal,
    Not,
    And,
    Or,
}

impl TryFrom<Expr> for Query {
    type Error = InvalidSearchError;

    fn try_from(value: Expr) -> Result<Self, InvalidSearchError> {
        expr_to_query(value)
    }
}

fn expr_to_query(expr: Expr) -> Result<Query, InvalidSearchError> {
    println!("Expr: {:?}", expr);

    match expr {
        Expr::Sequence(mut seq) if seq.len() == 1 => Ok(expr_to_query(seq.pop().unwrap())?),
        Expr::Sequence(mut seq) => {
            let has_or = seq.iter().find(|expr| matches!(expr, Expr::Or)).is_some();

            if has_or {
                let mut root: Vec<Query> = Vec::new();
                let mut current: Vec<Query> = Vec::new();

                // let not_state =

                for expr in seq.into_iter() {}

                Ok(Query::Or(vec![]))
            } else {
                let mut root: Vec<Query> = Vec::new();
                let mut not_state = false;

                for expr in seq.into_iter() {
                    if not_state {
                        root.push(Query::Not(Box::new(expr_to_query(expr)?)));
                        not_state = false;
                    } else if matches!(expr, Expr::Not) {
                        not_state = true;
                    } else if matches!(expr, Expr::And) {
                        // Do nothing.
                    } else {
                        root.push(expr_to_query(expr)?);
                    }
                }

                Ok(vec_to_and_query(root))
            }
        }
        Expr::String(string) => Ok(Query::Keyword(string)),
        Expr::Filter { key, op, value } => Ok(Query::Filter { key, op, value }),
        Expr::Property { key, op, value } => Ok(Query::Property { key, op, value }),
        x => {
            println!("Not recognized: {:?}", x);
            Err(InvalidSearchError)
        } //         _ => Err(InvalidSearchError),
    }
}

fn vec_to_and_query(vec: Vec<Query>) -> Query {
    if vec.len() == 1 {
        vec.into_iter().next().unwrap()
    } else {
        Query::And(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asdf() {
        let input = "aaa && (bb && !cc)";

        println!("Result: {:?}", parse(input));

        panic!();
    }
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
