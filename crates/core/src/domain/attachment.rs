// ============================================================
//  Attachment
// ============================================================

use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

/// The attachment that belongs to an [`Entry`].
#[derive(Clone, TypedBuilder, Debug, Serialize, Deserialize)]
pub struct Attachment {
    /// The ID of attachment.
    #[builder(setter(into))]
    pub id: AttachmentId,

    /// The name of attachment.
    #[builder(setter(into))]
    pub name: String,

    /// The relative path of file on the disk.
    #[builder(setter(into))]
    pub file_path: PathBuf,

    /// The size of file.
    pub size: u64,

    /// The create time.
    pub create_time: OffsetDateTime,

    /// The last updated time.
    pub update_time: OffsetDateTime,
}

impl Default for Attachment {
    fn default() -> Self {
        Self {
            id: Default::default(),
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

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
pub struct AttachmentId(pub String);

impl<T> From<T> for AttachmentId
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
