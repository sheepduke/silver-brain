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
        request: &CreateItemLinkRequest,
    ) -> silver_brain_core::ServiceResponse<()> {
        let parent = ItemId::from_str(&request.parent)?;
        let child = ItemId::from_str(&request.child)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                if repo::item_link::exists(&mut (*tx), &parent, &child).await? {
                    Ok(())
                } else if repo::item_link::exists(&mut (*tx), &child, &parent).await? {
                    Err(ServiceError::InvalidArgument(format!(
                        "`{}` is already a parent of `{}`",
                        request.child, request.parent
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
        request: &DeleteItemLinkRequest,
    ) -> ServiceResponse<()> {
        let parent = ItemId::from_str(&request.parent)?;
        let child = ItemId::from_str(&request.child)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                repo::item_link::delete(tx, &parent, &child).await
            })
            .await
    }
}
