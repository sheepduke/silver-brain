use typed_builder::TypedBuilder;

// ============================================================
//  RequestContext
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct RequestContext {
    #[builder(setter(into))]
    pub store_name: StoreName,
}

// ============================================================
//  StoreName
// ============================================================

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StoreName(pub String);

impl<T> From<T> for StoreName
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

impl Default for StoreName {
    fn default() -> Self {
        Self("default".to_string())
    }
}

// ============================================================
//  Error
// ============================================================

#[derive(Debug)]
pub enum ServiceError {
    NotFound,

    BadArguments,

    InvalidStoreName,

    InvalidAttachmentFilePath,

    Other(anyhow::Error),
}

pub type ServiceResult<T> = Result<T, ServiceError>;

// impl From<anyhow::Error> for ServiceError {
//     fn from(value: anyhow::Error) -> Self {
//         Self::Other(value)
//     }
// }

impl<T: std::error::Error + Send + Sync + 'static> From<T> for ServiceError {
    fn from(value: T) -> Self {
        Self::Other(anyhow::Error::new(value))
    }
}
