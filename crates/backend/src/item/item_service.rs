use std::str::FromStr;

use silver_brain_core::*;
use time::OffsetDateTime;

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
                let item_opt = repo::item::get(&mut *tx, &item_id, options).await?;

                if let Some(mut item) = item_opt {
                    if options.load_properties {
                        item.properties =
                            Some(repo::item_property::get_all(&mut *tx, &item.id).await?)
                    }

                    if options.load_parents {
                        item.parents =
                            Some(repo::item_link::get_parents(&mut *tx, &item.id).await?);
                    }

                    if options.load_children {
                        item.children =
                            Some(repo::item_link::get_children(&mut *tx, &item.id).await?)
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
        request: CreateItemRequest,
    ) -> ServiceResponse<ItemId> {
        let current_time = OffsetDateTime::now_utc();

        let item = Item::builder()
            .id(ItemId::new())
            .name(&request.name)
            .content_type(request.content_type.unwrap_or_default())
            .content(request.content.unwrap_or_default())
            .create_time(current_time)
            .update_time(current_time)
            .build();

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                repo::item::insert(tx, &item).await
            })
            .await?;

        Ok(item.id)
    }

    async fn update_item(
        &self,
        context: &RequestContext,
        request: UpdateItemRequest,
    ) -> ServiceResponse<()> {
        let item_id = ItemId::from_str(&request.id)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                let item_opt =
                    repo::item::get(&mut *tx, &item_id, &ItemLoadOptions::core()).await?;

                match item_opt {
                    Some(mut item) => {
                        if let Some(name) = request.name.clone() {
                            item.name = name;
                        }

                        if let Some(content_type) = request.content_type.clone() {
                            item.content_type = Some(content_type);
                        }

                        if let Some(content) = request.content.clone() {
                            item.content = Some(content);
                        }

                        item.update_time = Some(OffsetDateTime::now_utc());

                        repo::item::update(&mut *tx, &item).await
                    }

                    None => Err(ServiceError::InvalidArgument(format!(
                        "Invalid item id {}",
                        &item_id
                    ))),
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

    async fn upsert_item_property(
        &self,
        context: &RequestContext,
        request: UpsertItemPropertyRequest,
    ) -> ServiceResponse<()> {
        let item_id = request.item_id.parse()?;
        let property = ItemProperty {
            key: request.key,
            value: request.value,
        };

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                if repo::item_property::exists(&mut *tx, &item_id, &property.key).await? {
                    repo::item_property::update(&mut *tx, &item_id, &property).await
                } else {
                    repo::item_property::insert(&mut *tx, &item_id, &property).await
                }
            })
            .await
    }

    async fn delete_item_property(
        &self,
        context: &RequestContext,
        item_id: &str,
        key: &str,
    ) -> ServiceResponse<()> {
        let item_id = ItemId::from_str(item_id)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                repo::item_property::delete(tx, &item_id, &key).await
            })
            .await
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use anyhow::Result;
    use silver_brain_core::{
        CreateItemRequest, Item, ItemId, ItemLoadOptions, ItemProperty, ItemService,
        RequestContext, UpdateItemRequest, UpsertItemPropertyRequest,
    };

    use crate::{SqlService, repo::InMemorySqliteConnector};

    #[tokio::test]
    async fn create() -> Result<()> {
        let (service, context, item_id) = create_all().await?;

        let load_options = ItemLoadOptions::builder()
            .load_content_type(true)
            .load_content(true)
            .build();

        let item_opt = service.get_item(&context, &item_id, &load_options).await?;

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
        let (service, context, item_id) = create_all().await?;

        let request = UpdateItemRequest::builder()
            .id(item_id.as_str())
            .name("Test 2")
            .content("New content")
            .build();

        service.update_item(&context, request).await?;

        let load_options = ItemLoadOptions::builder().load_content(true).build();
        let item = service
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
        let (service, context, item_id) = create_all().await?;

        service.delete_item(&context, item_id.as_str()).await?;

        let item_opt = service
            .get_item(&context, item_id.as_str(), &ItemLoadOptions::all())
            .await?;

        assert_eq!(item_opt, None);

        Ok(())
    }

    #[tokio::test]
    async fn upsert_item_property() -> Result<()> {
        let (service, context, item_id) = create_all().await?;

        let request = UpsertItemPropertyRequest::builder()
            .item_id(item_id.to_string())
            .key("key")
            .value("value")
            .build();

        service.upsert_item_property(&context, request).await?;

        let properties = service
            .get_item(
                &context,
                item_id.as_str(),
                &ItemLoadOptions::builder().load_properties(true).build(),
            )
            .await?
            .unwrap()
            .properties
            .unwrap();

        assert_eq!(properties.len(), 1);
        assert_eq!(
            properties.first().unwrap().to_owned(),
            ItemProperty::builder().key("key").value("value").build()
        );

        Ok(())
    }

    #[tokio::test]
    async fn delete_item_property() -> Result<()> {
        let (service, context, item_id) = create_all().await?;

        let request = UpsertItemPropertyRequest::builder()
            .item_id(item_id.to_string())
            .key("key")
            .value("value")
            .build();

        service.upsert_item_property(&context, request).await?;

        service
            .delete_item_property(&context, item_id.as_str(), "key")
            .await?;

        let properties = service
            .get_item(
                &context,
                item_id.as_str(),
                &ItemLoadOptions::builder().load_properties(true).build(),
            )
            .await?
            .unwrap()
            .properties
            .unwrap();

        assert_eq!(properties.len(), 0);
        assert!(properties.first().is_none());

        Ok(())
    }

    async fn create_all() -> Result<(impl ItemService, RequestContext, ItemId)> {
        let service = create_service()?;
        let context = create_request_context()?;
        let request = CreateItemRequest::builder()
            .name("Test")
            .content_type("text/plain")
            .content("Hello")
            .build();

        let item_id = service.create_item(&context, request).await?;

        Ok((service, context, item_id))
    }

    pub(crate) fn create_service() -> Result<impl ItemService> {
        let session = Arc::new(InMemorySqliteConnector::new()?);

        Ok(SqlService::new(session))
    }

    pub(crate) fn create_request_context() -> Result<RequestContext> {
        Ok(RequestContext {
            repo_name: "main".parse()?,
        })
    }
}
