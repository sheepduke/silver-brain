use anyhow::Result;
use silver_brain_core::{
    EntryCreateRequest, EntryId, EntryService, EntryTagCreateRequest, RequestContext,
};

pub async fn create_editor(
    service: &impl EntryService,
    context: &RequestContext,
) -> Result<EntryId> {
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
) -> Result<EntryId> {
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

    service
        .create_entry_tag(
            &context,
            EntryTagCreateRequest::builder()
                .entry_id(entry_id.clone())
                .name("open-source")
                .build(),
        )
        .await?;

    service
        .create_entry_tag(
            &context,
            EntryTagCreateRequest::builder()
                .entry_id(entry_id.clone())
                .name("GNU")
                .build(),
        )
        .await?;

    Ok(entry_id)
}

pub async fn create_vim(service: &impl EntryService, context: &RequestContext) -> Result<EntryId> {
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

    service
        .create_entry_tag(
            &context,
            EntryTagCreateRequest::builder()
                .entry_id(entry_id.clone())
                .name("vi")
                .build(),
        )
        .await?;

    Ok(entry_id)
}

pub async fn create_neovim(
    service: &impl EntryService,
    context: &RequestContext,
) -> Result<EntryId> {
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

    service
        .create_entry_tag(
            &context,
            EntryTagCreateRequest::builder()
                .entry_id(entry_id.clone())
                .name("open-source")
                .build(),
        )
        .await?;

    service
        .create_entry_tag(
            &context,
            EntryTagCreateRequest::builder()
                .entry_id(entry_id.clone())
                .name("vi")
                .build(),
        )
        .await?;

    Ok(entry_id)
}
