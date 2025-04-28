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
    use silver_brain_core::{CreateItemRequest, ItemLoadOptions, ItemService, RequestContext};

    use crate::{repo::InMemoryStoreSession, service::item::SqlItemService};

    #[tokio::test]
    async fn crud() -> Result<()> {
        let session = Arc::new(InMemoryStoreSession::new()?);
        let item_service = SqlItemService::new(session);

        let context = RequestContext {
            store_name: "main".to_string(),
        };

        let request = CreateItemRequest::builder()
            .name("Test")
            .content_type("text/plain")
            .content("Hello")
            .build();

        let item_id = item_service.create_item(&context, &request).await?;

        let load_options = ItemLoadOptions::builder()
            .load_content_type(true)
            .load_content(true)
            .build();

        let item = item_service
            .get_item(&context, item_id.as_str(), &load_options)
            .await?
            .unwrap();

        assert_eq!(item.id, item_id);
        assert_eq!(item.name, "Test".to_string());
        assert_eq!(item.content_type, Some("text/plain".to_string()));
        assert_eq!(item.content, Some("Hello".to_string()));

        Ok(())
    }
}
