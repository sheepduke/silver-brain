use anyhow::Result;
use async_trait::async_trait;
use sea_orm::{ActiveValue, DatabaseConnection, EntityTrait};
use svix_ksuid::{Ksuid, KsuidLike};
use typed_builder::TypedBuilder;

use crate::{
    store::{entity, Store},
    AttachmentId, Entry, EntryId, EntryTagId, RequestContext, ToIso8601String,
};

use super::entry_service::{
    AttachmentCreateRequest, AttachmentUpdateRequest, EntryCreateRequest, EntryLoadOption,
    EntryService, EntryUpdateRequest,
};

#[derive(TypedBuilder, Debug)]
pub struct SqlEntryService<S: Store<DatabaseConnection>> {
    store: S,
}

impl<S: Store<DatabaseConnection>> SqlEntryService<S> {
    pub fn new(store: S) -> Self {
        Self { store }
    }
}

#[async_trait]
impl<S: Store<DatabaseConnection>> EntryService for SqlEntryService<S> {
    async fn create_entry(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> Result<EntryId> {
        let conn = self.store.get_conn(&context.store_name).await?;

        let uid = Ksuid::new(None, None);
        let now_time_string = uid.timestamp().to_iso_8601_string();

        let record = entity::entry::ActiveModel {
            id: ActiveValue::set(uid.to_string()),
            name: ActiveValue::set(request.name),
            content_type: ActiveValue::set(request.content_type.unwrap_or_default()),
            content: ActiveValue::set(request.content.unwrap_or_default()),
            create_time: ActiveValue::set(now_time_string.clone()),
            update_time: ActiveValue::set(now_time_string),
        };

        entity::entry::Entity::insert(record).exec(&conn).await?;

        Ok(uid.to_string().into())
    }

    async fn get_entry(
        &self,
        context: &RequestContext,
        id: &EntryId,
        option: &EntryLoadOption,
    ) -> Result<Entry> {
        todo!()
    }

    async fn get_entries(
        &self,
        context: &RequestContext,
        ids: &[EntryId],
        option: &EntryLoadOption,
    ) -> Result<Vec<Entry>> {
        todo!()
    }

    async fn update_entry(
        &self,
        context: &RequestContext,
        request: EntryUpdateRequest,
    ) -> Result<()> {
        todo!()
    }

    async fn delete_entry(&self, context: &RequestContext, id: &EntryId) -> Result<()> {
        todo!()
    }

    async fn create_entry_tag(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> Result<EntryTagId> {
        todo!()
    }

    async fn delete_entry_tag(&self, context: &RequestContext, id: &EntryTagId) -> Result<()> {
        todo!()
    }

    async fn create_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentCreateRequest,
    ) -> Result<AttachmentId> {
        todo!()
    }

    async fn update_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentUpdateRequest,
    ) -> Result<()> {
        todo!()
    }

    async fn delete_attachment(&self, context: &RequestContext, id: &AttachmentId) -> Result<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, str::FromStr};

    use crate::store::SqliteStore;

    use super::*;

    // #[tokio::test]
    async fn asdf() {
        let store =
            SqliteStore::new(&PathBuf::from_str("/home/invalid/temp/silver-brain").unwrap())
                .unwrap();
        let service = SqlEntryService::new(store);

        let context = RequestContext::builder().store_name("default").build();

        let request = EntryCreateRequest::builder()
            .name("Test")
            .content_type("text/md")
            .content("What??")
            .build();

        service.create_entry(&context, request).await;
    }
}
