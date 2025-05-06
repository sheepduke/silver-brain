use std::str::FromStr;

use silver_brain_core::*;

use crate::repo::{self, DatabaseConnector};

use super::SqlService;

impl<C> ItemService for SqlService<C>
where
    C: DatabaseConnector,
{
    async fn get_item(
        &self,
        context: &RequestContext,
        id: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Option<Item>> {
        let item_id = ItemId::from_str(id)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                let item_opt = repo::item::get(&mut (*tx), &item_id, options).await?;

                if let Some(mut item) = item_opt {
                    if options.load_parents {
                        item.parents =
                            Some(repo::item_link::get_parents(&mut (*tx), &item.id).await?);
                    }

                    if options.load_children {
                        item.children =
                            Some(repo::item_link::get_children(&mut (*tx), &item.id).await?)
                    }

                    Ok(Some(item))
                } else {
                    Ok(None)
                }
            })
            .await
    }

    async fn create_item(
        &self,
        context: &RequestContext,
        request: &CreateItemRequest,
    ) -> ServiceResponse<ItemId> {
        let id = ItemId::new();

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
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

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
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

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
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

    use crate::{SqlService, repo::InMemorySqliteConnector};

    type InMemoryItemService = SqlService<InMemorySqliteConnector>;

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
        let context = create_request_context()?;
        let request = CreateItemRequest::builder()
            .name("Test")
            .content_type("text/plain")
            .content("Hello")
            .build();

        let item_id = item_service.create_item(&context, &request).await?;

        Ok((item_service, context, item_id))
    }

    fn create_item_service() -> Result<impl ItemService> {
        let session = Arc::new(InMemorySqliteConnector::new()?);

        Ok(SqlService::new(session))
    }

    fn create_request_context() -> Result<RequestContext> {
        Ok(RequestContext {
            repo_name: "main".parse()?,
        })
    }
}
