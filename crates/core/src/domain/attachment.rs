// ============================================================
//  Attachment
// ============================================================

use std::path::PathBuf;

use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use crate::EntryId;

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
