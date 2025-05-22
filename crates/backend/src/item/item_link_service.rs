use std::str::FromStr;

use silver_brain_core::*;

use crate::{
    DatabaseConnector,
    repo::{self, ToServiceResponse},
};

use super::SqlService;

impl<C> ItemLinkService for SqlService<C>
where
    C: DatabaseConnector + Send + Sync,
{
    async fn create_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> silver_brain_core::ServiceResponse<()> {
        let parent = parent.parse()?;
        let child = child.parse()?;

        let mut conn = self.connector.begin_transaction(&context.repo_name).await?;

        if parent == child {
            Err(ServiceError::Conflict(
                "Cannot create a loop link".to_string(),
            ))
        } else if repo::item_link::exists(&mut conn, &parent, &child).await? {
            Ok(())
        } else if repo::item_link::exists(&mut conn, &child, &parent).await? {
            Err(ServiceError::Conflict(format!(
                "`{}` is already a parent of `{}`",
                child.as_str(),
                parent.as_str()
            )))
        } else {
            repo::item_link::insert(&mut conn, &parent, &child).await?;
            conn.commit().await.to_service_response()
        }
    }

    async fn delete_link(
        &self,
        context: &RequestContext,
        parent: &str,
        child: &str,
    ) -> ServiceResponse<()> {
        let parent = ItemId::from_str(parent)?;
        let child = ItemId::from_str(child)?;

        let mut conn = self.connector.get_connection(&context.repo_name).await?;

        repo::item_link::delete(&mut conn, &parent, &child).await
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use silver_brain_core::*;
    use std::{str::FromStr, sync::Arc};

    use crate::{InMemorySqliteConnector, SqlService, item::util::tests::setup};

    #[tokio::test]
    async fn create_and_delete_link() -> Result<()> {
        // Setup.
        let service = SqlService::new(Arc::new(InMemorySqliteConnector::new()?));

        let context = RequestContext::builder()
            .repo_name(RepoName::from_str("main")?)
            .build();

        let request = CreateItemRequest::builder().name("parent").build();
        let parent_id = service.create_item(&context, request).await?;

        let request = CreateItemRequest::builder().name("child").build();
        let child_id = service.create_item(&context, request).await?;

        service
            .create_link(&context, parent_id.as_str(), child_id.as_str())
            .await?;

        // Assert parents / children are set for both.
        let parent = service
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

        let child = service
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
        let result = service
            .create_link(&context, parent_id.as_str(), child_id.as_str())
            .await;

        assert!(result.is_ok());

        // Try to add an invalid one and got failure.
        let result = service
            .create_link(&context, child_id.as_str(), parent_id.as_str())
            .await;

        assert!(result.is_err());

        // Delete link and assert again.
        service
            .delete_link(&context, parent_id.as_str(), child_id.as_str())
            .await?;

        let parent = service
            .get_item(&context, parent_id.as_str(), &ItemLoadOptions::all())
            .await?
            .unwrap();

        assert_eq!(parent.children.unwrap().len(), 0);

        let child = service
            .get_item(&context, child_id.as_str(), &ItemLoadOptions::all())
            .await?
            .unwrap();

        assert_eq!(child.parents.unwrap().len(), 0);

        Ok(())
    }

    #[tokio::test]
    async fn create_link_self_loop() -> Result<()> {
        let (service, context, ids) = setup().await?;

        let result = service.create_link(&context, &ids.emacs, &ids.emacs).await;

        assert!(matches!(result, Err(ServiceError::Conflict(_))));

        Ok(())
    }
}
