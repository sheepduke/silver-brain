use std::str::FromStr;

use silver_brain_core::*;

use crate::{DatabaseConnector, repo};

use super::SqlService;

impl<C> ItemLinkService for SqlService<C>
where
    C: DatabaseConnector,
{
    async fn create_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> silver_brain_core::ServiceResponse<()> {
        let parent = ItemId::from_str(parent)?;
        let child = ItemId::from_str(child)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                if repo::item_link::exists(&mut (*tx), &parent, &child).await? {
                    Ok(())
                } else if repo::item_link::exists(&mut (*tx), &child, &parent).await? {
                    Err(ServiceError::InvalidArgument(format!(
                        "`{}` is already a parent of `{}`",
                        child.as_str(),
                        parent.as_str()
                    )))
                } else {
                    repo::item_link::insert(&mut (*tx), &parent, &child).await
                }
            })
            .await
    }

    async fn delete_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> ServiceResponse<()> {
        let parent = ItemId::from_str(parent)?;
        let child = ItemId::from_str(child)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                repo::item_link::delete(tx, &parent, &child).await
            })
            .await
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use silver_brain_core::*;
    use std::{str::FromStr, sync::Arc};

    use crate::{InMemorySqliteConnector, SqlService};

    #[tokio::test]
    async fn create_and_delete_link() -> Result<()> {
        // Setup.
        let item_service = SqlService::new(Arc::new(InMemorySqliteConnector::new()?));

        let context = RequestContext::builder()
            .repo_name(RepoName::from_str("main")?)
            .build();

        let request = CreateItemRequest::builder().name("parent").build();
        let parent_id = item_service.create_item(&context, request).await?;

        let request = CreateItemRequest::builder().name("child").build();
        let child_id = item_service.create_item(&context, request).await?;

        item_service
            .create_link(&context, parent_id.as_str(), child_id.as_str())
            .await?;

        // Assert parents / children are set for both.
        let parent = item_service
            .get_item(
                &context,
                parent_id.as_str(),
                &ItemLoadOptions::builder()
                    .load_parents(true)
                    .load_children(true)
                    .build(),
            )
            .await?
            .unwrap();

        assert_eq!(parent.parents.as_ref().unwrap().len(), 0);
        assert_eq!(parent.children.as_ref().unwrap().len(), 1);
        assert_eq!(
            parent.children.as_ref().unwrap().first().cloned().unwrap(),
            CoreItem::builder()
                .id(child_id.clone())
                .name("child")
                .build()
        );

        let child = item_service
            .get_item(
                &context,
                child_id.as_str(),
                &ItemLoadOptions::builder()
                    .load_parents(true)
                    .load_children(true)
                    .build(),
            )
            .await?
            .unwrap();

        assert_eq!(child.children.as_ref().unwrap().len(), 0);
        assert_eq!(child.parents.as_ref().unwrap().len(), 1);
        assert_eq!(
            child.parents.as_ref().unwrap().first().cloned().unwrap(),
            CoreItem::builder()
                .id(parent_id.clone())
                .name("parent")
                .build()
        );

        // Try to add a same one and got success.
        let result = item_service
            .create_link(&context, parent_id.as_str(), child_id.as_str())
            .await;

        assert!(result.is_ok());

        // Try to add an invalid one and got failure.
        let result = item_service
            .create_link(&context, child_id.as_str(), parent_id.as_str())
            .await;

        assert!(result.is_err());

        // Delete link and assert again.
        item_service
            .delete_link(&context, parent_id.as_str(), child_id.as_str())
            .await?;

        let parent = item_service
            .get_item(&context, parent_id.as_str(), &ItemLoadOptions::all())
            .await?
            .unwrap();

        assert_eq!(parent.children.unwrap().len(), 0);

        let child = item_service
            .get_item(&context, child_id.as_str(), &ItemLoadOptions::all())
            .await?
            .unwrap();

        assert_eq!(child.parents.unwrap().len(), 0);

        Ok(())
    }
}
