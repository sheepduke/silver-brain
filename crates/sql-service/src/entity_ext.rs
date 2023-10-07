use silver_brain_core::{Attachment, Entry, EntryTag, IntoOffsetDateTime};

use crate::entity;

impl From<entity::entry_tag::Model> for EntryTag {
    fn from(value: entity::entry_tag::Model) -> Self {
        Self::builder().id(value.id).name(value.name).build()
    }
}

impl From<entity::attachment::Model> for Attachment {
    fn from(value: entity::attachment::Model) -> Self {
        Self::builder()
            .id(value.id)
            .name(value.name)
            .entry_id(value.entry_id)
            .file_path(value.file_path)
            .size(value.size.try_into().unwrap())
            .create_time(value.create_time.into_offset_date_time())
            .update_time(value.update_time.into_offset_date_time())
            .build()
    }
}
