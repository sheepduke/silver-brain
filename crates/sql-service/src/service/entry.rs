use anyhow::Result;
use async_trait::async_trait;
use sea_orm::{
    ActiveModelTrait, ActiveValue, DatabaseConnection, EntityTrait, ModelTrait, PaginatorTrait,
};
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
        let conn = self.store.get_conn(&context.store_name).await?;

        let mut model: entity::entry::ActiveModel =
            entity::entry::Entity::find_by_id(&request.id.0)
                .one(&conn)
                .await?
                .ok_or(ClientError::IdNotFound)?
                .into();

        if let Some(name) = request.name {
            model.name = ActiveValue::Set(name);
        }

        if let Some(content_type) = request.content_type {
            model.content_type = ActiveValue::Set(content_type);
        }

        if let Some(content) = request.content {
            model.content = ActiveValue::Set(content);
        }

        model.update_time = ActiveValue::Set(OffsetDateTime::now_utc().to_iso_8601_string());

        model.update(&conn).await?;

        Ok(())
    }

    async fn delete_entry(&self, context: &RequestContext, id: &EntryId) -> Result<()> {
        let conn = self.store.get_conn(&context.store_name).await?;

        let _ = entity::entry::Entity::delete_by_id(&id.0)
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

        let model = entity::attachment::ActiveModel {
            id: ActiveValue::Set(id.to_string()),
            name: ActiveValue::Set(request.name),
            entry_id: ActiveValue::Set(request.entry_id.into()),
            size: ActiveValue::Set(0), // TODO: Read content length.
            file_path: ActiveValue::Set(path),
            create_time: ActiveValue::Set(now_time_string.clone()),
            update_time: ActiveValue::Set(now_time_string),
        };

        model.insert(&conn).await?;

        Ok(id.to_string().into())
    }

    async fn update_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentUpdateRequest,
    ) -> Result<()> {
        let conn = self.store.get_conn(&context.store_name).await?;

        let mut model: entity::attachment::ActiveModel =
            entity::attachment::Entity::find_by_id(&request.id)
                .one(&conn)
                .await?
                .ok_or(ClientError::IdNotFound)?
                .into();

        if let Some(entry_id) = request.entry_id {
            model.entry_id = ActiveValue::Set(entry_id.into());
        }

        if let Some(name) = request.name {
            model.name = ActiveValue::Set(name.clone());
        }

        model.update_time = ActiveValue::Set(OffsetDateTime::now_utc().to_iso_8601_string());

        model.update(&conn).await?;

        Ok(())
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
            .build();

        let entry = service.get_entry(&context, &id, &options).await.unwrap();

        assert_eq!(entry.content_type.unwrap(), "text/md");
        assert!(entry.content.unwrap().contains("Vim-based"));
        assert!(start_time <= entry.create_time.unwrap() && entry.create_time.unwrap() <= end_time);
        assert!(start_time <= entry.update_time.unwrap() && entry.update_time.unwrap() <= end_time);

        assert_eq!(entry.attachments.unwrap().iter().count(), 0);
    }

    fn create_service() -> SqlEntryService<SqliteStore> {
        let store = new_sqlite_store();
        SqlEntryService::new(store)
    }
}
