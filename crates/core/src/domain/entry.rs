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
    pub tags: Option<Vec<String>>,

    #[builder(default, setter(strip_option, into))]
    pub body: Option<EntryBody>,

    #[builder(default, setter(strip_option, into))]
    pub attachments: Option<Attachment>,

    #[builder(default, setter(strip_option, into))]
    pub metadata: Option<EntryMetadata>,
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
//  EntryBody
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryBody {
    #[builder(setter(into))]
    pub content_type: String,

    #[builder(setter(into))]
    pub content: String,
}

// ============================================================
//  EntryMetadata
// ============================================================

#[derive(Clone, TypedBuilder, Debug)]
pub struct EntryMetadata {
    pub create_time: OffsetDateTime,
    pub update_time: OffsetDateTime,
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
