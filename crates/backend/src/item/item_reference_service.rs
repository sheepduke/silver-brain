use std::str::FromStr;

use silver_brain_core::*;
use time::OffsetDateTime;

use crate::{
    DatabaseConnector,
    repo::{self, ToServiceResponse},
};

use super::SqlService;

impl<C> ItemReferenceService for SqlService<C>
where
    C: DatabaseConnector + Send + Sync,
{
    async fn get_source_references(
        &self,
        context: &RequestContext,
        source: &str,
    ) -> ServiceResponse<Vec<ItemReference>> {
        let source_id = ItemId::from_str(source)?;

        let mut conn = self.connector.get_connection(&context.repo_name).await?;

        repo::item_reference::get_source(&mut conn, &source_id).await
    }

    async fn get_target_references(
        &self,
        context: &RequestContext,
        target: &str,
    ) -> ServiceResponse<Vec<ItemReference>> {
        let target_id = ItemId::from_str(target)?;

        let mut conn = self.connector.get_connection(&context.repo_name).await?;

        repo::item_reference::get_target(&mut conn, &target_id).await
    }

    async fn create_reference(
        &self,
        context: &RequestContext,
        request: CreateItemReferenceRequest,
    ) -> ServiceResponse<ItemReferenceId> {
        let reference_id = ItemReferenceId::new();
        let source_id = ItemId::from_str(&request.source)?;
        let target_id = ItemId::from_str(&request.target)?;
        let current_time = OffsetDateTime::now_utc();

        let reference = ItemReference::builder()
            .id(reference_id)
            .source(source_id)
            .target(target_id)
            .annotation(request.annotation)
            .create_time(current_time)
            .update_time(current_time)
            .build();

        let mut conn = self.connector.get_connection(&context.repo_name).await?;

        repo::item_reference::insert(&mut conn, &reference).await?;

        Ok(reference.id)
    }

    async fn update_reference(
        &self,
        context: &RequestContext,
        request: UpdateItemReferenceRequest,
    ) -> ServiceResponse<()> {
        let reference_id = ItemReferenceId::from_str(&request.id)?;
        let current_time = OffsetDateTime::now_utc();

        let mut conn = self.connector.begin_transaction(&context.repo_name).await?;

        let reference = repo::item_reference::get(&mut conn, &reference_id).await?;

        match reference {
            Some(mut reference) => {
                if let Some(annotation) = request.annotation {
                    reference.annotation = annotation;
                }

                reference.update_time = current_time;

                repo::item_reference::update(&mut conn, &reference).await?;
                conn.commit().await.to_service_response()
            }
            None => Err(ServiceError::InvalidArgument("Not found".to_string())),
        }
    }

    async fn delete_reference(&self, context: &RequestContext, id: &str) -> ServiceResponse<()> {
        let reference_id = ItemReferenceId::from_str(id)?;

        let mut conn = self.connector.get_connection(&context.repo_name).await?;

        repo::item_reference::delete(&mut conn, &reference_id).await
    }
}

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use silver_brain_core::{
        CreateItemReferenceRequest, ItemLoadOptions, ItemReferenceService, ItemSearchService,
        UpdateItemReferenceRequest,
    };

    use crate::item::util::tests::setup;

    #[tokio::test]
    async fn crud() -> Result<()> {
        let (service, context, ids) = setup().await?;
        let load_options = ItemLoadOptions::all();

        // Create reference and get it.
        let request = CreateItemReferenceRequest::builder()
            .source(&ids.emacs)
            .target(&ids.software)
            .annotation("Is a")
            .build();

        let reference_id = service.create_reference(&context, request).await?;

        let references = service.get_source_references(&context, &ids.emacs).await?;

        let reference = references.first().unwrap();

        assert_eq!(reference.id, reference_id);
        assert_eq!(reference.annotation, "Is a");
        assert_eq!(reference.source.as_str(), &ids.emacs);
        assert_eq!(reference.target.as_str(), &ids.software);

        let references = service
            .get_target_references(&context, &ids.software)
            .await?;

        let reference = references.first().unwrap();

        assert_eq!(reference.id, reference_id);
        assert_eq!(reference.annotation, "Is a");
        assert_eq!(reference.source.as_str(), ids.emacs);
        assert_eq!(reference.target.as_str(), ids.software);

        // Update reference and get it.
        let request = UpdateItemReferenceRequest::builder()
            .id(reference_id.as_str())
            .annotation("New".to_string())
            .build();

        service.update_reference(&context, request).await?;

        let references = service.get_source_references(&context, &ids.emacs).await?;

        let reference = references.first().unwrap();

        assert_eq!(reference.id, reference_id);
        assert_eq!(reference.annotation, "New");
        assert_eq!(reference.source.as_str(), ids.emacs);
        assert_eq!(reference.target.as_str(), ids.software);

        let references = service
            .get_target_references(&context, &ids.software)
            .await?;

        let reference = references.first().unwrap();

        assert_eq!(reference.id, reference_id);
        assert_eq!(reference.annotation, "New");
        assert_eq!(reference.source.as_str(), ids.emacs);
        assert_eq!(reference.target.as_str(), ids.software);

        // Delete reference and get it.
        service
            .delete_reference(&context, reference_id.as_str())
            .await?;

        let references = service.get_source_references(&context, &ids.emacs).await?;

        assert!(references.is_empty());

        let references = service
            .get_target_references(&context, &ids.software)
            .await?;

        assert!(references.is_empty());

        Ok(())
    }
}
