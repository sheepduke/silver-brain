use std::error::Error;

use super::{
    expr::{parse_expr, Expr},
    query::Query,
};

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
        Ok(expr_to_query(expr)?)
    }
}

fn expr_to_query(expr: Expr) -> Result<Query, InvalidSearchError> {
    match expr {
        Expr::Sequence(mut seq) if seq.len() == 1 => Ok(expr_to_query(seq.pop().unwrap())?),
        Expr::Sequence(seq) => {
            let has_or = seq.iter().find(|expr| matches!(expr, Expr::Or)).is_some();

            if has_or {
                let mut root: Vec<Query> = Vec::new();
                let mut current: Vec<Query> = Vec::new();
                let mut not_state = false;

                for expr in seq.into_iter() {
                    if not_state {
                        current.push(Query::Not(Box::new(expr_to_query(expr)?)));
                        not_state = false;
                    } else if matches!(expr, Expr::Not) {
                        not_state = true
                    } else if matches!(expr, Expr::And) {
                        // Do nothing.
                    } else if matches!(expr, Expr::Or) {
                        root.push(vec_to_and_query(current));
                        current = Vec::new();
                    } else {
                        current.push(expr_to_query(expr)?);
                    }
                }

                if !current.is_empty() {
                    root.push(vec_to_and_query(current));
                }

                Ok(vec_to_or_query(root))
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
        }
    }
}

fn parse_filter_query(expr: Expr) -> Query {
    match expr {
        Expr::Filter { key, op, value } => Query::Filter {
            key: key,
            op: op,
            value: value,
        },
        _ => unreachable!(),
    }
}

fn vec_to_and_query(vec: Vec<Query>) -> Query {
    if vec.len() == 1 {
        vec.into_iter().next().unwrap()
    } else {
        Query::And(vec)
    }
}

fn vec_to_or_query(vec: Vec<Query>) -> Query {
    if vec.len() == 1 {
        vec.into_iter().next().unwrap()
    } else {
        Query::Or(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        assert_eq!(parse("aa").unwrap(), Query::Keyword("aa".to_string()));

        assert_eq!(
            parse(r#""aa bb""#).unwrap(),
            Query::Keyword("aa bb".to_string())
        );

        assert_eq!(parse("key = value"))
    }
}
