use typed_builder::TypedBuilder;

// ============================================================
//  RequestContext
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct RequestContext {
    pub store_name: StoreName,
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
