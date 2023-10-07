use anyhow::Result;
use async_trait::async_trait;
use sea_orm::{ActiveValue, DatabaseConnection, EntityTrait, ModelTrait, PaginatorTrait};
//use silver_brain_core::{EntryId, EntryService, RequestContext, EntryCreateRequest, EntryLoadOptions, Entry, EntryUpdateRequest, EntryTagId, AttachmentCreateRequest, AttachmentId, AttachmentUpdateRequest};
use silver_brain_core::*;
use svix_ksuid::{Ksuid, KsuidLike};
use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use crate::{
    entity,
    store::{SqliteStore, Store},
};

#[derive(TypedBuilder, Debug)]
pub struct SqlEntryService<S: Store<DatabaseConnection> = SqliteStore> {
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

        Ok(entity::entry::Entity::find().count(&conn).await?)
    }

    async fn create_entry(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> Result<EntryId> {
        let conn = self.create_conn(context).await?;

        let now_time = OffsetDateTime::now_utc();
        let uid = Ksuid::new(Some(now_time), None);
        let now_time_string = now_time.to_iso_8601_string();

        let record = entity::entry::ActiveModel {
            id: ActiveValue::set(uid.to_string()),
            name: ActiveValue::set(request.name),
            content_type: ActiveValue::set(request.content_type.unwrap_or_default()),
            content: ActiveValue::set(request.content.unwrap_or_default()),
            create_time: ActiveValue::set(now_time_string.clone()),
            update_time: ActiveValue::set(now_time_string),
        };

        entity::entry::Entity::insert(record).exec(&conn).await?;

        Ok(EntryId::new(uid.to_string()))
    }

    async fn get_entry(
        &self,
        context: &RequestContext,
        id: &EntryId,
        options: &EntryLoadOptions,
    ) -> Result<Entry> {
        let conn = self.create_conn(context).await?;

        let entry_entity = entity::entry::Entity::find_by_id(&id.0)
            .one(&conn)
            .await?
            .ok_or(ClientError::IdNotFound)?;

        let mut entry = Entry::builder()
            .id(entry_entity.id.clone())
            .name(entry_entity.name.clone())
            .build();

        if options.load_tags {
            entry.tags = Some(
                entry_entity
                    .find_related(entity::entry_tag::Entity)
                    .all(&conn)
                    .await?
                    .into_iter()
                    .map(|x| x.into())
                    .collect::<Vec<EntryTag>>(),
            );
        }

        if options.load_attachments {
            entry.attachments = Some(
                entry_entity
                    .find_related(entity::attachment::Entity)
                    .all(&conn)
                    .await?
                    .into_iter()
                    .map(|x| x.into())
                    .collect::<Vec<Attachment>>(),
            )
        }

        if options.load_content {
            entry.content_type = Some(entry_entity.content_type);
            entry.content = Some(entry_entity.content);
        }

        if options.load_times {
            entry.create_time = Some(OffsetDateTime::from_iso_8601_string(
                &entry_entity.create_time,
            )?);

            entry.update_time = Some(OffsetDateTime::from_iso_8601_string(
                &entry_entity.update_time,
            )?);
        }

        Ok(entry)
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
    async fn create_entry() {
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

    #[tokio::test]
    async fn get_entry() {
        let service = create_service();
        let context = RequestContext::default();

        let request = EntryCreateRequest::builder()
            .name("Test")
            .content_type("text/md")
            .content("What??")
            .build();

        let start_time = OffsetDateTime::now_utc();
        let id = service.create_entry(&context, request).await.unwrap();
        let end_time = OffsetDateTime::now_utc();

        let options = EntryLoadOptions::builder()
            .load_tags(true)
            .load_content(true)
            .load_times(true)
            .build();

        let entry = service.get_entry(&context, &id, &options).await.unwrap();
        assert_eq!(entry.tags.unwrap().iter().count(), 0);
        assert_eq!(entry.content_type.unwrap(), "text/md");
        assert_eq!(entry.content.unwrap(), "What??");
        assert!(start_time <= entry.create_time.unwrap() && entry.create_time.unwrap() <= end_time);
        assert!(start_time <= entry.update_time.unwrap() && entry.update_time.unwrap() <= end_time);
    }

    fn create_service() -> SqlEntryService<SqliteStore> {
        let store = new_sqlite_store();
        SqlEntryService::new(store)
    }
}
