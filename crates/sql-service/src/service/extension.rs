use anyhow::anyhow;
use silver_brain_core::{
    domain::Attachment,
    service::{ServiceError, ServiceResult},
};
use time::{format_description::well_known::Iso8601, OffsetDateTime};

use crate::entity;

impl From<entity::attachment::Model> for Attachment {
    fn from(value: entity::attachment::Model) -> Self {
        Self::builder()
            .id(value.id)
            .name(value.name)
            .file_path(value.file_path)
            .size(value.size.try_into().unwrap())
            .create_time(OffsetDateTime::from_iso_8601_string(&value.create_time).unwrap())
            .update_time(OffsetDateTime::from_iso_8601_string(&value.update_time).unwrap())
            .build()
    }
}

// ============================================================
//  OffsetDateTime
// ============================================================

pub trait OffsetDateTimeExtension {
    fn to_iso_8601_string(&self) -> String;

    fn from_iso_8601_string(input: &str) -> ServiceResult<OffsetDateTime>;
}

impl OffsetDateTimeExtension for OffsetDateTime {
    fn to_iso_8601_string(&self) -> String {
        self.format(&Iso8601::DEFAULT).unwrap()
    }

    fn from_iso_8601_string(input: &str) -> ServiceResult<Self> {
        Ok(Self::parse(input, &Iso8601::DEFAULT)
            .map_err(|_| ServiceError::Other(anyhow!("Cannot convert string to date time")))?)
    }
}
