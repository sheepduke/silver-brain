use silver_brain_core::{Attachment, IntoOffsetDateTime};

use crate::entity;

impl From<entity::attachment::Model> for Attachment {
    fn from(value: entity::attachment::Model) -> Self {
        Self::builder()
            .id(value.id)
            .name(value.name)
            .file_path(value.file_path)
            .size(value.size.try_into().unwrap())
            .create_time(value.create_time.into_offset_date_time())
            .update_time(value.update_time.into_offset_date_time())
            .build()
    }
}
