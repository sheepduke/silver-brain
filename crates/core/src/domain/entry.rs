use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use crate::Attachment;

// ============================================================
//  Entry
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct Entry {
    #[builder(setter(into))]
    pub id: EntryId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(strip_option, into))]
    pub tags: Option<Vec<EntryTag>>,

    #[builder(default, setter(strip_option, into))]
    pub content_type: Option<String>,

    #[builder(default, setter(strip_option, into))]
    pub content: Option<String>,

    #[builder(default, setter(strip_option))]
    pub attachments: Option<Vec<Attachment>>,

    #[builder(default, setter(strip_option, into))]
    pub create_time: Option<OffsetDateTime>,

    #[builder(default, setter(strip_option, into))]
    pub update_time: Option<OffsetDateTime>,
}

// ============================================================
//  EntryId
// ============================================================

#[derive(Clone, Default, Debug)]
pub struct EntryId(pub String);

impl EntryId {
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }
}

impl From<String> for EntryId {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for EntryId {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<EntryId> for String {
    fn from(value: EntryId) -> Self {
        value.0
    }
}

impl From<&EntryId> for String {
    fn from(value: &EntryId) -> Self {
        value.0.clone()
    }
}

// ============================================================
//  EntryTag
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryTag {
    #[builder(setter(into))]
    pub id: EntryTagId,

    #[builder(setter(into))]
    pub name: String,
}

// ============================================================
//  EntryTagId
// ============================================================

#[derive(Clone, Default, Debug)]
pub struct EntryTagId(pub String);

impl From<String> for EntryTagId {
    fn from(value: String) -> Self {
        Self(value)
    }
}
