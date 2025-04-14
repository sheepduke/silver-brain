#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FilterOperator {
    Filter,
    LessThan,
    LessEqual,
    Match,
    Equal,
    NotEqual,
    GreaterEqual,
    GreaterThan,
}
