use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{i32, space0},
    combinator::{map, value},
};

use super::{
    CompareOperator, SearchQuery,
    basic_query::{any_string, compare_operator, filter_operator, ord_operator},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FilterQuery {
    Name {
        operator: CompareOperator,
        value: String,
    },
    ContentType {
        operator: CompareOperator,
        value: String,
    },
    Content {
        operator: CompareOperator,
        value: String,
    },
    HasProperty {
        value: String,
    },
    ParentsCount {
        operator: CompareOperator,
        value: i32,
    },
    ChildrenCount {
        operator: CompareOperator,
        value: i32,
    },
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
        "name" => SearchQuery::Filter(FilterQuery::Name { operator, value }),
        "content_type" => SearchQuery::Filter(FilterQuery::ContentType { operator, value }),
        "content" => SearchQuery::Filter(FilterQuery::Content { operator, value }),
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
            "parents_count" => SearchQuery::Filter(FilterQuery::ParentsCount { operator, value }),
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

    let mapper = |(_, _, _, _, value)| SearchQuery::Filter(FilterQuery::HasProperty { value });

    map(pattern, mapper).parse(input)
}

pub fn filter_query(input: &str) -> IResult<&str, SearchQuery> {
    alt((
        filter_string_query,
        filter_int_query,
        filter_has_property_query,
    ))
    .parse(input)
}

#[cfg(test)]
mod tests {
    use nom::error::Error;

    use super::*;

    use FilterQuery as FQ;
    use SearchQuery as SQ;

    impl FilterQuery {
        pub fn name(operator: CompareOperator, value: impl Into<String>) -> SQ {
            SQ::Filter(FQ::Name {
                operator,
                value: value.into(),
            })
        }

        pub fn content_type(operator: CompareOperator, value: impl Into<String>) -> SQ {
            SQ::Filter(FQ::ContentType {
                operator,
                value: value.into(),
            })
        }

        pub fn content(operator: CompareOperator, value: impl Into<String>) -> SQ {
            SQ::Filter(FQ::Content {
                operator,
                value: value.into(),
            })
        }

        pub fn has_property(value: impl Into<String>) -> SQ {
            SQ::Filter(FQ::HasProperty {
                value: value.into(),
            })
        }

        pub fn parents_count(operator: CompareOperator, value: i32) -> SQ {
            SQ::Filter(FQ::ParentsCount { operator, value })
        }

        pub fn children_count(operator: CompareOperator, value: i32) -> SQ {
            SQ::Filter(FQ::ChildrenCount { operator, value })
        }
    }

    #[test]
    fn test_filter_name_query() {
        assert_eq!(
            filter_query("name : \"value\""),
            Ok(("", FQ::name(CompareOperator::Filter, "value")))
        );

        assert_eq!(
            filter_query("NAME=value"),
            Ok(("", FQ::name(CompareOperator::Equal, "value")))
        );

        assert_eq!(
            filter_query("name>=value"),
            Ok(("", FQ::name(CompareOperator::GreaterEqual, "value")))
        );
    }

    #[test]
    fn test_filter_content_type_query() {
        assert_eq!(
            filter_query("content-type: value"),
            Ok(("", FQ::content_type(CompareOperator::Filter, "value")))
        );
    }

    #[test]
    fn test_filter_parents_count_query() {
        assert_eq!(
            filter_query("parentsCOUNT >= 3"),
            Ok(("", FQ::parents_count(CompareOperator::GreaterEqual, 3)))
        );
    }

    #[test]
    fn test_filter_has_property_query() {
        assert_eq!(
            filter_query("has: value"),
            Ok(("", FQ::has_property("value")))
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
