use nom::{
    IResult, Parser,
    branch::alt,
    bytes::{
        complete::{escaped_transform, tag, take_while1},
        is_not,
    },
    character::{complete::space0, one_of},
    combinator::{map, value},
    sequence::delimited,
};

use super::filter_query::FilterQuery;

// ============================================================
//  Model
// ============================================================

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SearchQuery {
    And(Vec<SearchQuery>),
    Or(Vec<SearchQuery>),
    Not(Box<SearchQuery>),
    Keyword(String),
    Property {
        key: String,
        operator: CompareOperator,
        value: String,
    },
    Filter(FilterQuery),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompareOperator {
    Filter,
    LessThan,
    LessEqual,
    Match,
    Equal,
    NotEqual,
    GreaterEqual,
    GreaterThan,
}

// ============================================================
//  String Parser
// ============================================================

pub fn basic_string(input: &str) -> IResult<&str, String> {
    let is_basic_char = |c: char| !" &|!:~<=>())\"".contains(c);

    map(take_while1(is_basic_char), String::from).parse(input)
}

pub fn quoted_string(input: &str) -> IResult<&str, String> {
    delimited(
        tag("\""),
        escaped_transform(is_not("\\\""), '\\', one_of("\\\"")),
        tag("\""),
    )
    .parse(input)
}

pub fn any_string(input: &str) -> IResult<&str, String> {
    alt((basic_string, quoted_string)).parse(input)
}

// ============================================================
//  Compare Operator Parser
// ============================================================

pub fn filter_operator(input: &str) -> IResult<&str, CompareOperator> {
    value(CompareOperator::Filter, tag(":")).parse(input)
}

pub fn ord_operator(input: &str) -> IResult<&str, CompareOperator> {
    alt((
        value(CompareOperator::NotEqual, tag("!=")),
        value(CompareOperator::Equal, tag("=")),
        value(CompareOperator::LessEqual, tag("<=")),
        value(CompareOperator::LessThan, tag("<")),
        value(CompareOperator::GreaterEqual, tag(">=")),
        value(CompareOperator::GreaterThan, tag(">")),
    ))
    .parse(input)
}

pub fn match_operator(input: &str) -> IResult<&str, CompareOperator> {
    value(CompareOperator::Match, tag("~")).parse(input)
}

pub fn compare_operator(input: &str) -> IResult<&str, CompareOperator> {
    alt((filter_operator, ord_operator, match_operator)).parse(input)
}

// ============================================================
//  Query Parser
// ============================================================

pub fn keyword_query(input: &str) -> IResult<&str, SearchQuery> {
    map(any_string, SearchQuery::Keyword).parse(input)
}

// ============================================================
//  Custom Property Query
// ============================================================

pub fn property_query(input: &str) -> IResult<&str, SearchQuery> {
    let mapper = |(_, key, _, operator, _, value)| SearchQuery::Property {
        key,
        operator,
        value,
    };

    map(
        (
            tag("$"),
            any_string,
            space0,
            compare_operator,
            space0,
            any_string,
        ),
        mapper,
    )
    .parse(input)
}

// ============================================================
//  Tests
// ============================================================

#[cfg(test)]
mod tests {
    use crate::FilterQuery;

    use super::CompareOperator as CO;
    use super::SearchQuery as SQ;
    use super::*;

    impl SearchQuery {
        pub fn keyword(value: impl Into<String>) -> Self {
            Self::Keyword(value.into())
        }

        pub fn not(value: SearchQuery) -> Self {
            Self::Not(Box::new(value))
        }

        pub fn property(
            key: impl Into<String>,
            operator: CompareOperator,
            value: impl Into<String>,
        ) -> Self {
            Self::Property {
                key: key.into(),
                operator,
                value: value.into(),
            }
        }
    }

    #[test]
    fn test_basic_string() {
        assert_eq!(basic_string("hello").unwrap(), ("", "hello".to_string()));

        assert_eq!(
            basic_string("hello world").unwrap(),
            (" world", "hello".to_string())
        );

        assert!(basic_string("!aa").is_err());
    }

    #[test]
    fn test_quoted_string() {
        assert_eq!(
            quoted_string(r#""hello, world""#).unwrap(),
            ("", "hello, world".to_string())
        );

        assert_eq!(
            quoted_string(r#""aa\\bb\"cc\"dd""#).unwrap(),
            ("", r#"aa\bb"cc"dd"#.to_string())
        )
    }

    #[test]
    fn test_any_string() {
        assert_eq!(any_string("hello").unwrap(), ("", "hello".to_string()));

        assert_eq!(any_string(r#""hello""#).unwrap(), ("", "hello".to_string()));
    }

    #[test]
    fn test_compare_operator() {
        assert_eq!(compare_operator(":").unwrap(), ("", CO::Filter));

        assert_eq!(compare_operator("<=").unwrap(), ("", CO::LessEqual));

        assert_eq!(compare_operator("<").unwrap(), ("", CO::LessThan));

        assert_eq!(compare_operator("=").unwrap(), ("", CO::Equal));

        assert_eq!(compare_operator("!=").unwrap(), ("", CO::NotEqual));

        assert_eq!(compare_operator(">").unwrap(), ("", CO::GreaterThan));

        assert_eq!(compare_operator(">=").unwrap(), ("", CO::GreaterEqual));
    }

    #[test]
    fn test_keyword_query() {
        assert_eq!(keyword_query("aaa"), Ok(("", SQ::keyword("aaa"))));

        assert_eq!(keyword_query("aa bb"), Ok((" bb", SQ::keyword("aa"))));
    }

    #[test]
    fn test_property_query() {
        assert_eq!(
            property_query("$aa < bb"),
            Ok(("", SQ::property("aa", CO::LessThan, "bb")))
        );

        assert_eq!(
            property_query(r#"$aa  ~ "bb" "#),
            Ok((" ", SQ::property("aa", CO::Match, "bb")))
        );
    }
}
