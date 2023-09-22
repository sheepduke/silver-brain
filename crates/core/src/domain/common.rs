use anyhow::Result;
use time::{format_description::well_known::Iso8601, OffsetDateTime};
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
//  OffsetDateTime
// ============================================================

pub trait ToIso8601String {
    fn to_iso_8601_string(&self) -> String;
}

pub trait FromIso8601String<Out = Self> {
    fn from_iso_8601_string(input: &str) -> Result<Out>;
}

impl ToIso8601String for OffsetDateTime {
    fn to_iso_8601_string(&self) -> String {
        self.format(&Iso8601::DEFAULT).unwrap()
    }
}

impl FromIso8601String for OffsetDateTime {
    fn from_iso_8601_string(input: &str) -> Result<Self> {
        Ok(Self::parse(input, &Iso8601::DEFAULT)?)
    }
}
