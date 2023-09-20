use anyhow::Result;
use async_trait::async_trait;
use typed_builder::TypedBuilder;

use crate::{store::Store, AttachmentId, Entry, EntryId, EntryTagId, RequestContext};

use super::entry_service::{
    AttachmentCreateRequest, AttachmentUpdateRequest, EntryCreateRequest, EntryLoadOption,
    EntryService, EntryUpdateRequest,
};

#[derive(Default, TypedBuilder, Debug)]
pub struct SqliteEntryService<S: Store> {
    store: S,
}

#[async_trait]
impl<S: Store> EntryService for SqliteEntryService<S> {
    async fn create_entry(
        &self,
        context: &RequestContext,
        request: &EntryCreateRequest,
    ) -> Result<EntryId> {
        todo!()
    }

    async fn get_entry(
        &self,
        context: &RequestContext,
        id: &EntryId,
        option: &EntryLoadOption,
    ) -> Result<Entry> {
        todo!()
    }

    async fn get_entries(
        &self,
        context: &RequestContext,
        ids: &[EntryId],
        option: &EntryLoadOption,
    ) -> Result<Vec<Entry>> {
        todo!()
    }

    async fn update_entry(
        &self,
        context: &RequestContext,
        request: &EntryUpdateRequest,
    ) -> Result<()> {
        todo!()
    }

    async fn delete_entry(&self, context: &RequestContext, id: &EntryId) -> Result<()> {
        todo!()
    }

    async fn create_entry_tag(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> Result<EntryTagId> {
        todo!()
    }

    async fn delete_entry_tag(&self, context: &RequestContext, id: &EntryTagId) -> Result<()> {
        todo!()
    }

    async fn create_attachment(
        &self,
        context: &RequestContext,
        request: &AttachmentCreateRequest,
    ) -> Result<AttachmentId> {
        todo!()
    }

    async fn update_attachment(
        &self,
        context: &RequestContext,
        request: &AttachmentUpdateRequest,
    ) -> Result<()> {
        todo!()
    }

    async fn delete_attachment(&self, context: &RequestContext, id: &AttachmentId) -> Result<()> {
        todo!()
    }
}
