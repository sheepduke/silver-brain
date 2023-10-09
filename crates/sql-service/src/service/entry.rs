use anyhow::Result;
use async_trait::async_trait;
use sea_orm::{ActiveValue, DatabaseConnection, EntityTrait, ModelTrait, PaginatorTrait};
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
        let conn = self.store.get_conn(&context.store_name).await?;

        let _ = entity::entry::Entity::delete_by_id(&id.0)
            .exec(&conn)
            .await?;

        Ok(())
    }

    async fn create_entry_tag(
        &self,
        context: &RequestContext,
        request: EntryTagCreateRequest,
    ) -> Result<EntryTagId> {
        let conn = self.store.get_conn(&context.store_name).await?;
        let tag_id = Ksuid::new(None, None);

        let entity = entity::entry_tag::ActiveModel {
            id: ActiveValue::set(tag_id.to_string()),
            name: ActiveValue::set(request.name),
            entry_id: ActiveValue::set(request.entry_id.into()),
        };

        entity::entry_tag::Entity::insert(entity)
            .exec(&conn)
            .await?;

        Ok(tag_id.to_string().into())
    }

    async fn delete_entry_tag(&self, context: &RequestContext, id: &EntryTagId) -> Result<()> {
        let conn = self.store.get_conn(&context.store_name).await?;
        let _ = entity::entry_tag::Entity::delete_by_id(&id.0)
            .exec(&conn)
            .await?;

        Ok(())
    }

    async fn create_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentCreateRequest,
    ) -> Result<AttachmentId> {
        // Check file path first.
        let path = match request.file_path.to_str() {
            Some(path) if request.file_path.exists() && request.file_path.is_file() => Ok(path),
            _ => Err(ClientError::InvalidFilePath),
        }?
        .to_string();

        let conn = self.store.get_conn(&context.store_name).await?;
        let now_time = OffsetDateTime::now_utc();
        let now_time_string = now_time.to_iso_8601_string();
        let id = Ksuid::new(Some(now_time), None);

        let entity = entity::attachment::ActiveModel {
            id: ActiveValue::Set(id.to_string()),
            name: ActiveValue::Set(request.name),
            entry_id: ActiveValue::Set(request.entry_id.into()),
            size: ActiveValue::Set(0), // TODO: Read content length.
            file_path: ActiveValue::Set(path),
            create_time: ActiveValue::Set(now_time_string.clone()),
            update_time: ActiveValue::Set(now_time_string),
        };

        entity::attachment::Entity::insert(entity)
            .exec(&conn)
            .await?;

        Ok(id.to_string().into())
    }

    async fn update_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentUpdateRequest,
    ) -> Result<()> {
        todo!()
    }

    async fn delete_attachment(&self, context: &RequestContext, id: &AttachmentId) -> Result<()> {
        let conn = self.store.get_conn(&context.store_name).await?;
        let _ = entity::attachment::Entity::delete_by_id(&id.0)
            .exec(&conn)
            .await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::store::{new_sqlite_store, SqliteStore};
    use silver_brain_dev as dev;

    use super::*;

    #[tokio::test]
    async fn create_entry() {
        let service = create_service();
        let context = RequestContext::default();

        let _ = dev::create_editor(&service, &context).await;

        assert_eq!(service.count_entries(&context).await.unwrap(), 1);
    }

    #[tokio::test]
    async fn get_entry() {
        let service = create_service();
        let context = RequestContext::default();

        let start_time = OffsetDateTime::now_utc();
        let id = dev::create_neovim(&service, &context).await.unwrap();
        let end_time = OffsetDateTime::now_utc();

        let options = EntryLoadOptions::builder()
            .load_content(true)
            .load_times(true)
            .load_attachments(true)
            .load_tags(true)
            .build();

        let entry = service.get_entry(&context, &id, &options).await.unwrap();

        assert_eq!(entry.content_type.unwrap(), "text/md");
        assert!(entry.content.unwrap().contains("Vim-based"));
        assert!(start_time <= entry.create_time.unwrap() && entry.create_time.unwrap() <= end_time);
        assert!(start_time <= entry.update_time.unwrap() && entry.update_time.unwrap() <= end_time);

        let tags = entry.tags.unwrap();
        assert_eq!(tags.iter().count(), 2);
        assert!(tags.iter().find(|x| x.name == "open-source").is_some());
        assert!(tags.iter().find(|x| x.name == "vi").is_some());
        assert!(tags.iter().find(|x| x.name == "vim").is_none());

        assert_eq!(entry.attachments.unwrap().iter().count(), 0);
    }

    fn create_service() -> SqlEntryService<SqliteStore> {
        let store = new_sqlite_store();
        SqlEntryService::new(store)
    }
}
