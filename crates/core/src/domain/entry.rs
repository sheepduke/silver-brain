use serde::{Deserialize, Serialize};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use crate::Attachment;

// ============================================================
//  Entry
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug, Serialize, Deserialize)]
pub struct Entry {
    #[builder(setter(into))]
    pub id: EntryId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(strip_option, into))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,

    #[builder(default, setter(strip_option, into))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attachments: Option<Vec<Attachment>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<Vec<EntryProperty>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parents: Option<Vec<Entry>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<Entry>>,

    #[builder(default, setter(strip_option))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub friends: Option<Vec<Entry>>,

    #[builder(default, setter(strip_option, into))]
    #[serde(
        skip_serializing_if = "Option::is_none",
        with = "time::serde::rfc3339::option"
    )]
    pub create_time: Option<OffsetDateTime>,

    #[builder(default, setter(strip_option, into))]
    #[serde(
        skip_serializing_if = "Option::is_none",
        with = "time::serde::rfc3339::option"
    )]
    pub update_time: Option<OffsetDateTime>,
}

// ============================================================
//  EntryId
// ============================================================

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
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
//  EntryProperty
// ============================================================

#[derive(Clone, TypedBuilder, Debug, Serialize, Deserialize)]
pub struct EntryProperty {
    #[builder(setter(into))]
    pub id: EntryPropertyId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(setter(into))]
    pub value: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EntryPropertyId(pub String);
