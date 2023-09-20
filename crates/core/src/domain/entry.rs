use std::path::PathBuf;
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

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

impl<T> From<T> for EntryId
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
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

impl<T> From<T> for EntryTagId
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

// ============================================================
//  Attachment
// ============================================================

#[derive(Clone, TypedBuilder, Debug)]
pub struct Attachment {
    #[builder(setter(into))]
    pub id: AttachmentId,

    #[builder(setter(into))]
    pub entry_id: EntryId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(setter(into))]
    pub file_path: PathBuf,

    pub size: u64,
    pub create_time: OffsetDateTime,
    pub update_time: OffsetDateTime,
}

impl Default for Attachment {
    fn default() -> Self {
        Self {
            id: Default::default(),
            entry_id: Default::default(),
            name: Default::default(),
            file_path: Default::default(),
            size: 0,
            create_time: OffsetDateTime::now_utc(),
            update_time: OffsetDateTime::now_utc(),
        }
    }
}

// ============================================================
//  AttachmentId
// ============================================================

#[derive(Clone, Default, Debug)]
pub struct AttachmentId(pub String);

impl<T> From<T> for AttachmentId
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
