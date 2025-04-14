mod filter;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::{
        complete::{escaped_transform, tag, tag_no_case, take_while1},
        is_not,
    },
    character::{complete::i32, complete::space0, one_of},
    combinator::{map, value},
    sequence::delimited,
};

// ============================================================
//  Model
// ============================================================

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SearchQuery {
    Keyword(String),
    CustomProperty {
        key: String,
        operator: CompareOperator,
        value: String,
    },
    FilterName {
        operator: CompareOperator,
        value: String,
    },
    FilterContentType {
        operator: CompareOperator,
        value: String,
    },
    FilterContent {
        operator: CompareOperator,
        value: String,
    },
    FilterHasProperty {
        value: String,
    },
    FilterParentsCount {
        operator: CompareOperator,
        value: i32,
    },
    FilterChildrenCount {
        operator: CompareOperator,
        value: i32,
    },
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

fn basic_string(input: &str) -> IResult<&str, String> {
    let is_basic_char = |c: char| !" &|!:~<=>())\"".contains(c);

    map(take_while1(is_basic_char), String::from).parse(input)
}

fn quoted_string(input: &str) -> IResult<&str, String> {
    delimited(
        tag("\""),
        escaped_transform(is_not("\\\""), '\\', one_of("\\\"")),
        tag("\""),
    )
    .parse(input)
}

fn any_string(input: &str) -> IResult<&str, String> {
    alt((basic_string, quoted_string)).parse(input)
}

// ============================================================
//  Compare Operator Parser
// ============================================================

fn filter_operator(input: &str) -> IResult<&str, CompareOperator> {
    value(CompareOperator::Filter, tag(":")).parse(input)
}

fn ord_operator(input: &str) -> IResult<&str, CompareOperator> {
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

fn match_operator(input: &str) -> IResult<&str, CompareOperator> {
    value(CompareOperator::Match, tag("~")).parse(input)
}

fn compare_operator(input: &str) -> IResult<&str, CompareOperator> {
    alt((filter_operator, ord_operator, match_operator)).parse(input)
}

// ============================================================
//  Keyword Query Parser
// ============================================================

fn keyword_query(input: &str) -> IResult<&str, SearchQuery> {
    map(any_string, SearchQuery::Keyword).parse(input)
}

// ============================================================
//  Custom Property Query
// ============================================================

fn custom_property_query(input: &str) -> IResult<&str, SearchQuery> {
    let mapper = |(_, key, _, operator, _, value)| SearchQuery::CustomProperty {
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
//  Filter Query Parser
// ============================================================

fn filter_string_query(input: &str) -> IResult<&str, SearchQuery> {
    let pattern = (
        alt((
            tag_no_case("name"),
            value(
                "content_type",
                alt((
                    tag_no_case("content-type"),
                    tag_no_case("content_type"),
                    tag_no_case("contentType"),
                )),
            ),
            tag_no_case("content"),
        )),
        space0,
        compare_operator,
        space0,
        any_string,
    );

    let mapper = |(name, _, operator, _, value): (&str, _, CompareOperator, _, String)| match name
        .to_lowercase()
        .as_str()
    {
        "name" => SearchQuery::FilterName { operator, value },
        "content_type" => SearchQuery::FilterContentType { operator, value },
        "content" => SearchQuery::FilterContent { operator, value },
        _ => panic!("Unreachable variant"),
    };

    map(pattern, mapper).parse(input)
}

fn filter_int_query(input: &str) -> IResult<&str, SearchQuery> {
    let pattern = (
        alt((
            value(
                "parents_count",
                alt((
                    tag_no_case("parents-count"),
                    tag_no_case("parents_count"),
                    tag_no_case("parentsCount"),
                )),
            ),
            value(
                "children_count",
                alt((
                    tag_no_case("children-count"),
                    tag_no_case("children_count"),
                    tag_no_case("childrenCount"),
                )),
            ),
        )),
        space0,
        alt((filter_operator, ord_operator)),
        space0,
        i32,
    );

    let mapper =
        |(name, _, operator, _, value): (&str, _, _, _, _)| match name.to_lowercase().as_str() {
            "parents_count" => SearchQuery::FilterParentsCount { operator, value },
            _ => panic!("Unreachable variant"),
        };

    map(pattern, mapper).parse(input)
}

fn filter_has_property_query(input: &str) -> IResult<&str, SearchQuery> {
    let pattern = (
        tag_no_case("has"),
        space0,
        filter_operator,
        space0,
        any_string,
    );

    let mapper = |(_, _, _, _, value)| SearchQuery::FilterHasProperty { value };

    map(pattern, mapper).parse(input)
}

fn filter_query(input: &str) -> IResult<&str, SearchQuery> {
    alt((
        filter_string_query,
        filter_int_query,
        filter_has_property_query,
    ))
    .parse(input)
}

// ============================================================
//  Tests
// ============================================================

#[cfg(test)]
mod tests {
    use std::clone;

    use nom::error::{Error, ErrorKind};

    use super::*;

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
        assert_eq!(
            compare_operator(":").unwrap(),
            ("", CompareOperator::Filter)
        );

        assert_eq!(
            compare_operator("<=").unwrap(),
            ("", CompareOperator::LessEqual)
        );

        assert_eq!(
            compare_operator("<").unwrap(),
            ("", CompareOperator::LessThan)
        );

        assert_eq!(compare_operator("=").unwrap(), ("", CompareOperator::Equal));

        assert_eq!(
            compare_operator("!=").unwrap(),
            ("", CompareOperator::NotEqual)
        );

        assert_eq!(
            compare_operator(">").unwrap(),
            ("", CompareOperator::GreaterThan)
        );

        assert_eq!(
            compare_operator(">=").unwrap(),
            ("", CompareOperator::GreaterEqual)
        );
    }

    #[test]
    fn test_keyword_query() {
        assert_eq!(
            keyword_query("aaa").unwrap(),
            ("", SearchQuery::Keyword("aaa".to_string()))
        );

        assert_eq!(
            keyword_query("aa bb").unwrap(),
            (" bb", SearchQuery::Keyword("aa".to_string()))
        );
    }

    #[test]
    fn test_custom_property_query() {
        assert_eq!(
            custom_property_query("$aa < bb").unwrap(),
            (
                "",
                SearchQuery::CustomProperty {
                    key: "aa".to_string(),
                    operator: CompareOperator::LessThan,
                    value: "bb".to_string(),
                }
            )
        );

        assert_eq!(
            custom_property_query(r#"$aa  ~ "bb" "#).unwrap(),
            (
                " ",
                SearchQuery::CustomProperty {
                    key: "aa".to_string(),
                    operator: CompareOperator::Match,
                    value: "bb".to_string(),
                }
            )
        );
    }

    #[test]
    fn test_filter_name_query() {
        assert_eq!(
            filter_query("name : \"value\"").unwrap(),
            (
                "",
                SearchQuery::FilterName {
                    operator: CompareOperator::Filter,
                    value: "value".to_string()
                }
            )
        );

        assert_eq!(
            filter_query("NAME=value").unwrap(),
            (
                "",
                SearchQuery::FilterName {
                    operator: CompareOperator::Equal,
                    value: "value".to_string()
                }
            )
        );

        assert_eq!(
            filter_query("name>=value").unwrap(),
            (
                "",
                SearchQuery::FilterName {
                    operator: CompareOperator::GreaterEqual,
                    value: "value".to_string()
                }
            )
        );
    }

    #[test]
    fn test_filter_content_type_query() {
        assert_eq!(
            filter_query("content-type: value").unwrap(),
            (
                "",
                SearchQuery::FilterContentType {
                    operator: CompareOperator::Filter,
                    value: "value".to_string()
                }
            )
        );
    }

    #[test]
    fn test_filter_parents_count_query() {
        assert_eq!(
            filter_query("parentsCOUNT >= 3").unwrap(),
            (
                "",
                SearchQuery::FilterParentsCount {
                    operator: CompareOperator::GreaterEqual,
                    value: 3
                }
            )
        );
    }

    #[test]
    fn test_filter_has_property_query() {
        assert_eq!(
            filter_query("has: value").unwrap(),
            (
                "",
                SearchQuery::FilterHasProperty {
                    value: "value".to_string()
                }
            )
        );

        let result = filter_query("has = value");
        assert!(matches!(
            result,
            Err(nom::Err::Error(Error {
                input: "= value",
                code: _
            }))
        ));
    }
}
