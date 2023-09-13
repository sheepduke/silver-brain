// ============================================================
//  RequestContext
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct RequestContext {
    store_name: StoreName,
}

// ============================================================
//  StoreName
// ============================================================

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct StoreName(pub String);

impl<T> From<T> for StoreName
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

mod entry;
pub use entry::*;

mod entry_service;
pub use entry_service::*;
use typed_builder::TypedBuilder;
