use anyhow::Result;
use async_trait::async_trait;
use sea_orm::{ActiveValue, DatabaseConnection, EntityTrait, PaginatorTrait};
//use silver_brain_core::{EntryId, EntryService, RequestContext, EntryCreateRequest, EntryLoadOptions, Entry, EntryUpdateRequest, EntryTagId, AttachmentCreateRequest, AttachmentId, AttachmentUpdateRequest};
use silver_brain_core::*;
use svix_ksuid::{Ksuid, KsuidLike};
use typed_builder::TypedBuilder;

use crate::{entity::entry, store::Store};

#[derive(TypedBuilder, Debug)]
pub struct SqlEntryService<S: Store<DatabaseConnection>> {
    store: S,
}

impl<S: Store<DatabaseConnection>> SqlEntryService<S> {
    pub fn new(store: S) -> Self {
        Self { store }
    }

    async fn create_conn(&self, context: &RequestContext) -> Result<DatabaseConnection> {
        Ok(self.store.get_conn(&context.store_name).await?)
    }
}

#[async_trait]
impl<S: Store<DatabaseConnection>> EntryService for SqlEntryService<S> {
    async fn count_entries(&self, context: &RequestContext) -> Result<u64> {
        let conn = self.create_conn(context).await?;

        Ok(entry::Entity::find().count(&conn).await?)
    }

    async fn create_entry(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> Result<EntryId> {
        let conn = self.create_conn(context).await?;

        let uid = Ksuid::new(None, None);
        let now_time_string = uid.timestamp().to_iso_8601_string();

        let record = entry::ActiveModel {
            id: ActiveValue::set(uid.to_string()),
            name: ActiveValue::set(request.name),
            content_type: ActiveValue::set(request.content_type.unwrap_or_default()),
            content: ActiveValue::set(request.content.unwrap_or_default()),
            create_time: ActiveValue::set(now_time_string.clone()),
            update_time: ActiveValue::set(now_time_string),
        };

        entry::Entity::insert(record).exec(&conn).await?;

        Ok(EntryId::new(uid.to_string()))
    }

    async fn get_entry(
        &self,
        context: &RequestContext,
        id: &EntryId,
        options: &EntryLoadOptions,
    ) -> Result<Entry> {
        let conn = self.create_conn(context).await?;

        let entry = entry::Entity::find_by_id("something")
            .one(&conn)
            .await?
            .ok_or(ServiceError::IdNotFound)?;

        todo!()
    }

    async fn get_entries(
        &self,
        context: &RequestContext,
        ids: &[EntryId],
        options: &EntryLoadOptions,
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
    use crate::store::{new_sqlite_store, SqliteStore};

    use super::*;

    #[tokio::test]
    async fn asdf() {
        let service = create_service();

        let context = RequestContext::default();

        let request = EntryCreateRequest::builder()
            .name("Test")
            .content_type("text/md")
            .content("What??")
            .build();

        let _ = service.create_entry(&context, request).await.unwrap();
        assert_eq!(service.count_entries(&context).await.unwrap(), 1);
    }

    fn create_service() -> SqlEntryService<SqliteStore> {
        let store = new_sqlite_store();
        SqlEntryService::new(store)
    }
}
