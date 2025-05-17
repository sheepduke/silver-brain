mod sql_service;
pub use sql_service::SqlService;

mod item_link_service;
mod item_reference_service;
mod item_search_service;
mod item_service;

#[cfg(test)]
pub(crate) mod tests {
    use std::sync::Arc;

    use anyhow::Result;
    use silver_brain_core::*;

    use crate::{InMemorySqliteConnector, SqlService};

    pub async fn setup() -> Result<(SqlService<InMemorySqliteConnector>, RequestContext)> {
        let service = SqlService::new(Arc::new(InMemorySqliteConnector::new()?));
        let repo_name: RepoName = "main".parse()?;
        let context = RequestContext::builder().repo_name(repo_name).build();

        // Setup Software.
        let request = CreateItemRequest::builder().name("Software").build();
        service.create_item(&context, request).await?;

        // Setup Emacs.
        let request = CreateItemRequest::builder().name("Emacs").build();
        let emacs_id = service.create_item(&context, request).await?;

        let request = UpsertItemPropertyRequest::builder()
            .item_id(emacs_id.as_str())
            .key("type")
            .value("editor")
            .build();
        service.upsert_item_property(&context, request).await?;

        // Setup Vim.
        let request = CreateItemRequest::builder().name("Vim").build();
        let vim_id = service.create_item(&context, request).await?;

        let request = UpsertItemPropertyRequest::builder()
            .item_id(vim_id.as_str())
            .key("type")
            .value("editor")
            .build();
        service.upsert_item_property(&context, request).await?;

        // Setup Firefox.
        let request = CreateItemRequest::builder().name("Firefox").build();
        let firefox_id = service.create_item(&context, request).await?;

        let request = UpsertItemPropertyRequest::builder()
            .item_id(firefox_id.as_str())
            .key("type")
            .value("browser")
            .build();
        service.upsert_item_property(&context, request).await?;

        Ok((service, context))
    }
}
