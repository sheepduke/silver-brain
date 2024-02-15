mod expr;

mod parser;
pub use parser::{parse, InvalidSearchError};

mod query;
pub use query::{CompareOperator, Query};
