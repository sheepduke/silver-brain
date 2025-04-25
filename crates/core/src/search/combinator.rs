use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{space0, space1},
    combinator::{all_consuming, map},
    multi::separated_list1,
};

use crate::{ServiceError, ServiceResponse};

use super::{
    SearchQuery,
    basic_query::{keyword_query, property_query},
    filter_query::filter_query,
};

fn single(input: &str) -> IResult<&str, SearchQuery> {
    alt((property_query, filter_query, keyword_query)).parse(input)
}

fn group(input: &str) -> IResult<&str, SearchQuery> {
    map((tag("("), or_expr, tag(")")), |(_, it, _)| it).parse(input)
}

fn term(input: &str) -> IResult<&str, SearchQuery> {
    alt((group, single)).parse(input)
}

fn not_expr(input: &str) -> IResult<&str, SearchQuery> {
    alt((
        map((not_operator, term), |(_, it)| {
            SearchQuery::Not(Box::new(it))
        }),
        term,
    ))
    .parse(input)
}

fn not_operator(input: &str) -> IResult<&str, &str> {
    map((tag("!"), space0), |(_, _)| "").parse(input)
}

fn and_expr(input: &str) -> IResult<&str, SearchQuery> {
    map(
        separated_list1(alt((and_operator, space1)), not_expr),
        SearchQuery::And,
    )
    .parse(input)
}

fn and_operator(input: &str) -> IResult<&str, &str> {
    map((space0, tag("&&"), space0), |(_, _, _)| "").parse(input)
}

fn or_expr(input: &str) -> IResult<&str, SearchQuery> {
    map(separated_list1(or_operator, and_expr), |it| {
        SearchQuery::Or(it)
    })
    .parse(input)
}

fn or_operator(input: &str) -> IResult<&str, &str> {
    map((space0, tag("||"), space0), |(_, _, _)| "").parse(input)
}

fn flatten_search_query(query: SearchQuery) -> SearchQuery {
    match query {
        SearchQuery::Not(query) => SearchQuery::Not(Box::new(flatten_search_query(*query))),

        SearchQuery::And(vec) => {
            if vec.len() == 1 {
                flatten_search_query(vec.first().unwrap().to_owned())
            } else {
                SearchQuery::And(vec.into_iter().map(flatten_search_query).collect())
            }
        }

        SearchQuery::Or(vec) => {
            if vec.len() == 1 {
                flatten_search_query(vec.first().unwrap().to_owned())
            } else {
                SearchQuery::Or(vec.into_iter().map(flatten_search_query).collect())
            }
        }

        _ => query,
    }
}

pub fn parse(search: &str) -> ServiceResponse<SearchQuery> {
    match all_consuming(or_expr).parse(search.trim()) {
        Ok((_, query)) => Ok(flatten_search_query(query)),
        Err(error) => Err(ServiceError::InvalidArgument(format!(
            "Invalid search string: {}",
            error
        ))),
    }
}

#[cfg(test)]
mod tests {
    use crate::{CompareOperator, FilterQuery};

    use super::*;

    use CompareOperator as CO;
    use SearchQuery as SQ;

    #[test]
    fn test_basic_combniation() {
        assert_eq!(
            parse("aa !(bb ||  cc) && !dd ").unwrap(),
            SQ::And(vec![
                SQ::keyword("aa"),
                SQ::not(SQ::Or(vec![SQ::keyword("bb"), SQ::keyword("cc"),])),
                SQ::not(SQ::keyword("dd"))
            ])
        );
    }

    #[test]
    fn test_all_combination() {
        assert_eq!(
            parse("aa !name: bb $bb = cc").unwrap(),
            SQ::And(vec![
                SQ::keyword("aa"),
                SQ::not(SQ::Filter(FilterQuery::Name {
                    operator: CO::Filter,
                    value: "bb".to_string()
                })),
                SQ::property("bb", CO::Equal, "cc")
            ])
        );
    }
}
