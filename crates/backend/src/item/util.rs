#[cfg(test)]
pub(crate) mod tests {
    use std::sync::Arc;

    use anyhow::Result;
    use silver_brain_core::*;
    use typed_builder::TypedBuilder;

    use crate::{InMemorySqliteConnector, SqlService};

    #[derive(TypedBuilder)]
    pub(crate) struct ItemIds {
        #[builder(setter(into))]
        pub software: String,

        #[builder(setter(into))]
        pub emacs: String,

        #[builder(setter(into))]
        pub vim: String,

        #[builder(setter(into))]
        pub firefox: String,
    }

    pub async fn setup() -> Result<(SqlService<InMemorySqliteConnector>, RequestContext, ItemIds)> {
        let service = SqlService::new(Arc::new(InMemorySqliteConnector::new()?));
        let repo_name: RepoName = "main".parse()?;
        let context = RequestContext::builder().repo_name(repo_name).build();

        // Setup Software.
        let request = CreateItemRequest::builder().name("Software").build();
        let software_id = service.create_item(&context, request).await?;

        // Setup Emacs.
        let request = CreateItemRequest::builder().name("Emacs").build();
        let emacs_id = service.create_item(&context, request).await?;

        let request = UpsertItemPropertyRequest::builder()
            .key("type")
            .value("editor")
            .build();

        service
            .upsert_item_property(&context, &emacs_id, request)
            .await?;

        // Setup Vim.
        let request = CreateItemRequest::builder().name("Vim").build();
        let vim_id = service.create_item(&context, request).await?;

        let request = UpsertItemPropertyRequest::builder()
            .key("type")
            .value("editor")
            .build();

        service
            .upsert_item_property(&context, &vim_id, request)
            .await?;

        // Setup Firefox.
        let request = CreateItemRequest::builder().name("Firefox").build();
        let firefox_id = service.create_item(&context, request).await?;

        let request = UpsertItemPropertyRequest::builder()
            .key("type")
            .value("browser")
            .build();
        service
            .upsert_item_property(&context, &firefox_id, request)
            .await?;

        let item_ids = ItemIds::builder()
            .software(software_id)
            .emacs(emacs_id)
            .vim(vim_id)
            .firefox(firefox_id)
            .build();

        Ok((service, context, item_ids))
    }
}
