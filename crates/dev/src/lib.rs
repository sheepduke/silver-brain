use anyhow::Result;
use silver_brain_core::{
    domain::EntryId,
    service::{EntryCreateRequest, EntryService, RequestContext, ServiceResult},
};

pub async fn create_editor(
    service: &impl EntryService,
    context: &RequestContext,
) -> ServiceResult<EntryId> {
    let request = EntryCreateRequest::builder()
        .name("Editor")
        .content_type("text/plain")
        .content("")
        .build();

    Ok(service.create_entry(&context, request).await?)
}

pub async fn create_emacs(
    service: &impl EntryService,
    context: &RequestContext,
) -> ServiceResult<EntryId> {
    let entry_id = service
        .create_entry(
            &context,
            EntryCreateRequest::builder()
                .name("Emacs")
                .content_type("text/org")
                .content("Emacs editor")
                .build(),
        )
        .await?;

    Ok(entry_id)
}

pub async fn create_vim(
    service: &impl EntryService,
    context: &RequestContext,
) -> ServiceResult<EntryId> {
    let entry_id = service
        .create_entry(
            &context,
            EntryCreateRequest::builder()
                .name("Vim")
                .content_type("text/md")
                .content("Vi IMproved")
                .build(),
        )
        .await?;

    Ok(entry_id)
}

pub async fn create_neovim(
    service: &impl EntryService,
    context: &RequestContext,
) -> ServiceResult<EntryId> {
    let entry_id = service
        .create_entry(
            &context,
            EntryCreateRequest::builder()
                .name("Neovim")
                .content_type("text/md")
                .content("hyperextensible Vim-based text editor")
                .build(),
        )
        .await?;

    Ok(entry_id)
}
