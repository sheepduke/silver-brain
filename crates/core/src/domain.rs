use std::path::Path;
use time::OffsetDateTime;

#[derive(Debug)]
pub struct EntryId(String);

#[derive(Debug)]
pub struct LinkId(String);

#[derive(Debug)]
pub struct AttachmentId(String);

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Attachment {
    id: AttachmentId,
    entry_id: EntryId,
    name: String,
    file_path: Box<Path>,
    create_time: OffsetDateTime,
    update_time: OffsetDateTime,
}

#[derive(Debug)]
pub enum LinkType {
    Solid,
    Weak,
}

#[derive(Debug)]
pub struct Link {
    id: LinkId,
    source: EntryId,
    target: EntryId,
    annotation: String,
    link_type: LinkType,
}
