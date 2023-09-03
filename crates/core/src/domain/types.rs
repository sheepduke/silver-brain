use std::path::PathBuf;
use time::OffsetDateTime;

#[derive(Clone, Default, Debug)]
pub struct EntryId(String);

#[derive(Clone, Default, Debug)]
pub struct LinkId(String);

#[derive(Clone, Default, Debug)]
pub struct AttachmentId(String);

#[derive(Clone, Default, Debug)]
pub struct Entry {
    pub id: EntryId,
    pub name: String,
    pub tags: Option<Vec<String>>,
    pub content_type: Option<String>,
    pub content: Option<String>,
    pub attachments: Option<Attachment>,
    pub create_time: Option<OffsetDateTime>,
    pub update_time: Option<OffsetDateTime>,
}

#[derive(Clone, Debug)]
pub struct Attachment {
    pub id: AttachmentId,
    pub entry_id: EntryId,
    pub name: String,
    pub file_path: PathBuf,
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
            create_time: OffsetDateTime::now_utc(),
            update_time: OffsetDateTime::now_utc(),
        }
    }
}

#[derive(Clone, Default, Debug)]
pub enum LinkType {
    Solid,

    #[default]
    Weak,
}

#[derive(Clone, Default, Debug)]
pub struct Link {
    pub id: LinkId,
    pub source: EntryId,
    pub target: EntryId,
    pub annotation: String,
    pub link_type: LinkType,
}
