#[derive(PartialEq, Eq, Clone, Debug)]
pub enum CompareOperator {
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
}

#[derive(Clone, PartialEq, Eq, Debug)]
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum FilterKey {
    Has,
    ChildCount,
    LinkCount,
    ParentCount,
    CreateTime,
}
