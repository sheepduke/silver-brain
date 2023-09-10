use std::path::PathBuf;
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

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

#[derive(Clone, Default, Debug)]
pub struct LinkId(pub String);

impl<T> From<T> for LinkId
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

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

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct Entry {
    #[builder(setter(into))]
    pub id: EntryId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(into))]
    pub tags: Option<Vec<String>>,

    #[builder(default, setter(into))]
    pub body: Option<EntryBody>,

    #[builder(default, setter(into))]
    pub attachments: Option<Attachment>,

    #[builder(default, setter(into))]
    pub metadata: Option<EntryMetadata>,
}

#[derive(Clone, Default, Debug)]
pub struct EntryBody {
    pub content_type: String,
    pub content: String,
}

#[derive(Clone, Debug)]
pub struct EntryMetadata {
    pub create_time: OffsetDateTime,
    pub update_time: OffsetDateTime,
}

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryLoadOption {
    #[builder(setter(into))]
    pub load_tags: bool,

    #[builder(setter(into))]
    pub load_body: bool,

    #[builder(setter(into))]
    pub load_attachments: bool,

    #[builder(setter(into))]
    pub load_times: bool,
}

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

#[derive(Clone, Default, Debug)]
pub enum LinkType {
    Solid,

    #[default]
    Weak,
}

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct Link {
    #[builder(setter(into))]
    pub id: LinkId,

    #[builder(setter(into))]
    pub source: EntryId,

    #[builder(setter(into))]
    pub target: EntryId,

    #[builder(setter(into))]
    pub annotation: String,

    #[builder(setter(into))]
    pub link_type: LinkType,
}
