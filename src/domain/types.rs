use std::{collections::HashMap, path::Path};

use time::OffsetDateTime;

pub struct EntryId(String);

pub struct LinkId(String);

pub struct AttachmentId(String);

pub struct I18nStrings {
    strings: HashMap<&'static str, String>,
}

pub struct Entry {
    id: EntryId,
    name: I18nStrings,
    tags: Option<Vec<String>>,
    content_type: Option<String>,
    content: Option<I18nStrings>,
    attachments: Option<Attachment>,
    create_time: Option<OffsetDateTime>,
    update_time: Option<OffsetDateTime>,
}

pub struct Attachment {
    id: AttachmentId,
    entry_id: EntryId,
    name: I18nStrings,
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
    annotation: I18nStrings,
    link_type: LinkType,
}
