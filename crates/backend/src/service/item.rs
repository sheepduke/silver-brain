use std::str::FromStr;
use std::sync::Arc;

use silver_brain_core::*;

use crate::repo::{self, StoreSession};

pub struct SqlItemService<M>
where
    M: StoreSession,
{
    session: Arc<M>,
}

impl<M> SqlItemService<M>
where
    M: StoreSession,
{
    pub fn new(session: Arc<M>) -> Self {
        Self { session }
    }
}

impl<M> ItemService for SqlItemService<M>
where
    M: StoreSession,
{
    async fn get_item(
        &self,
        context: &RequestContext,
        id: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Option<Item>> {
        let item_id = ItemId::from_str(id)?;

        self.session
            .with_transaction(&context.store_name, async |tx| {
                repo::item::get(tx, &item_id, options).await
            })
            .await
    }

    async fn create_item(
        &self,
        context: &RequestContext,
        request: &CreateItemRequest,
    ) -> ServiceResponse<ItemId> {
        let id = ItemId::new();

        self.session
            .with_transaction(&context.store_name, async |tx| {
                repo::item::create(
                    tx,
                    &id,
                    &request.name,
                    &request.content_type,
                    &request.content,
                )
                .await
            })
            .await?;

        Ok(id)
    }

    async fn update_item(
        &self,
        context: &RequestContext,
        request: &UpdateItemRequest,
    ) -> ServiceResponse<()> {
        let item_id = ItemId::from_str(&request.id)?;

        self.session
            .with_transaction(&context.store_name, async |tx| {
                if repo::item::exists(&mut (*tx), &item_id).await? {
                    repo::item::update(
                        &mut (*tx),
                        &item_id,
                        request.name.as_deref(),
                        request.content_type.as_deref(),
                        request.content.as_deref(),
                    )
                    .await
                } else {
                    Err(ServiceError::InvalidArgument(format!(
                        "Invalid item id {}",
                        item_id.as_str()
                    )))
                }
            })
            .await
    }

    async fn delete_item(&self, context: &RequestContext, id: &str) -> ServiceResponse<()> {
        let item_id = ItemId::from_str(id)?;

        self.session
            .with_transaction(&context.store_name, async |tx| {
                repo::item::delete(tx, &item_id).await
            })
            .await
    }

    async fn upsert_property(
        &self,
        _context: &RequestContext,
        _urequest: &UpsertPropertyRequest,
    ) -> ServiceResponse<()> {
        todo!()
    }

    async fn delete_property(&self, _context: &RequestContext, _key: &str) -> ServiceResponse<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use anyhow::Result;
    use silver_brain_core::{
        CreateItemRequest, Item, ItemId, ItemLoadOptions, ItemService, RequestContext,
        UpdateItemRequest,
    };

    use crate::{repo::InMemoryStoreSession, service::item::SqlItemService};

    type InMemoryItemService = SqlItemService<InMemoryStoreSession>;

    #[tokio::test]
    async fn create() -> Result<()> {
        let (item_service, context, item_id) = create_all().await?;

        let load_options = ItemLoadOptions::builder()
            .load_content_type(true)
            .load_content(true)
            .build();

        let item_opt = item_service
            .get_item(&context, item_id.as_str(), &load_options)
            .await?;

        let expected = Item::builder()
            .id(item_id)
            .name("Test")
            .content_type("text/plain")
            .content("Hello")
            .build();

        assert_eq!(item_opt, Some(expected));

        Ok(())
    }

    #[tokio::test]
    async fn update() -> Result<()> {
        let (item_service, context, item_id) = create_all().await?;

        let request = UpdateItemRequest::builder()
            .id(item_id.as_str())
            .name("Test 2")
            .content("New content")
            .build();

        item_service.update_item(&context, &request).await?;

        let load_options = ItemLoadOptions::builder().load_content(true).build();
        let item = item_service
            .get_item(&context, item_id.as_str(), &load_options)
            .await?
            .unwrap();

        let expected = Item::builder()
            .id(item_id)
            .name("Test 2")
            .content("New content")
            .build();

        assert_eq!(item, expected);

        Ok(())
    }

    #[tokio::test]
    async fn delete() -> Result<()> {
        let (item_service, context, item_id) = create_all().await?;

        item_service.delete_item(&context, item_id.as_str()).await?;

        let item_opt = item_service
            .get_item(&context, item_id.as_str(), &ItemLoadOptions::all())
            .await?;

        assert_eq!(item_opt, None);

        Ok(())
    }

    async fn create_all() -> Result<(impl ItemService, RequestContext, ItemId)> {
        let item_service = create_item_service()?;
        let context = create_request_context();
        let request = CreateItemRequest::builder()
            .name("Test")
            .content_type("text/plain")
            .content("Hello")
            .build();

        let item_id = item_service.create_item(&context, &request).await?;

        Ok((item_service, context, item_id))
    }

    fn create_item_service() -> Result<impl ItemService> {
        let session = Arc::new(InMemoryStoreSession::new()?);

        Ok(SqlItemService::new(session))
    }

    fn create_request_context() -> RequestContext {
        RequestContext {
            store_name: "main".to_string(),
        }
    }
}
