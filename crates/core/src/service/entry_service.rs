use std::path::PathBuf;

use anyhow::Result;
use async_trait::async_trait;
use typed_builder::TypedBuilder;

use crate::{AttachmentId, Entry, EntryId, EntryTagId, RequestContext};

// ============================================================
//  EntryService
// ============================================================

#[async_trait]
pub trait EntryService {
    async fn create_entry(
        &self,
        context: &RequestContext,
        request: &EntryCreateRequest,
    ) -> Result<EntryId>;

    async fn get_entry(
        &self,
        context: &RequestContext,
        id: &EntryId,
        option: &EntryLoadOption,
    ) -> Result<Entry>;

    async fn get_entries(
        &self,
        context: &RequestContext,
        ids: &[EntryId],
        option: &EntryLoadOption,
    ) -> Result<Vec<Entry>>;

    async fn update_entry(
        &self,
        context: &RequestContext,
        request: &EntryUpdateRequest,
    ) -> Result<()>;

    async fn delete_entry(&self, context: &RequestContext, id: &EntryId) -> Result<()>;

    async fn create_entry_tag(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> Result<EntryTagId>;

    async fn delete_entry_tag(&self, context: &RequestContext, id: &EntryTagId) -> Result<()>;

    async fn create_attachment(
        &self,
        context: &RequestContext,
        request: &AttachmentCreateRequest,
    ) -> Result<AttachmentId>;

    async fn update_attachment(
        &self,
        context: &RequestContext,
        request: &AttachmentUpdateRequest,
    ) -> Result<()>;

    async fn delete_attachment(&self, context: &RequestContext, id: &AttachmentId) -> Result<()>;
}

// ============================================================
//  EntryLoadOption
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryLoadOption {
    #[builder(default, setter(into))]
    pub load_tags: bool,

    #[builder(default, setter(into))]
    pub load_body: bool,

    #[builder(default, setter(into))]
    pub load_attachments: bool,

    #[builder(default, setter(into))]
    pub load_times: bool,
}

// ============================================================
//  EntryCreateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryCreateRequest {
    #[builder(setter(into))]
    name: String,

    #[builder(default, setter(strip_option, into))]
    content_type: Option<String>,

    #[builder(default, setter(strip_option, into))]
    content: Option<String>,
}

// ============================================================
//  EntryUpdateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryUpdateRequest {
    #[builder(setter(into))]
    id: EntryId,

    #[builder(default, setter(strip_option, into))]
    name: Option<String>,

    #[builder(default, setter(strip_option, into))]
    content_type: Option<String>,

    #[builder(default, setter(strip_option, into))]
    content: Option<String>,
}

// ============================================================
//  EntryTagCreateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryTagCreateRequest {
    #[builder(setter(into))]
    entry_id: EntryId,

    #[builder(setter(into))]
    name: String,
}

// ============================================================
//  EntryTagUpdateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryTagUpdateRequest {
    #[builder(setter(into))]
    id: EntryTagId,

    #[builder(default, setter(strip_option, into))]
    name: Option<String>,
}

// ============================================================
//  AttachmentCreateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct AttachmentCreateRequest {
    #[builder(setter(into))]
    entry_id: EntryId,

    #[builder(setter(into))]
    name: String,

    #[builder(setter(into))]
    file_path: PathBuf,
}

// ============================================================
//  AttachmentUpdateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct AttachmentUpdateRequest {
    #[builder(setter(into))]
    id: EntryId,

    #[builder(default, setter(strip_option, into))]
    entry_id: Option<EntryId>,

    #[builder(default, setter(strip_option, into))]
    name: Option<String>,
}
