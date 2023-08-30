use std::{collections::HashMap, path::Path};
use time::OffsetDateTime;

pub struct EntryId(String);

pub struct LinkId(String);

pub struct AttachmentId(String);

pub struct Entry {
    id: EntryId,
    name: String,
    tags: Option<Vec<String>>,
    content_type: Option<String>,
    content: Option<String>,
    attachments: Option<Attachment>,
    create_time: Option<OffsetDateTime>,
    update_time: Option<OffsetDateTime>,
}

pub struct Attachment {
    id: AttachmentId,
    entry_id: EntryId,
    name: String,
    file_path: Box<Path>,
    create_time: OffsetDateTime,
    update_time: OffsetDateTime,
}

pub enum LinkType {
    Solid,
    Weak,
}

pub struct Link {
    id: LinkId,
    source: EntryId,
    target: EntryId,
    annotation: String,
    link_type: LinkType,
}
