use std::path::PathBuf;

use async_trait::async_trait;
use serde::Deserialize;
use typed_builder::TypedBuilder;

use crate::domain::{AttachmentId, Entry, EntryId};
use crate::service::{RequestContext, ServiceResult};

// ============================================================
//  EntryService
// ============================================================

#[async_trait]
pub trait EntryService {
    async fn count_entries(&self, context: &RequestContext) -> ServiceResult<u64>;

    async fn create_entry(
        &self,
        context: &RequestContext,
        request: EntryCreateRequest,
    ) -> ServiceResult<EntryId>;

    async fn get_entry(
        &self,
        context: &RequestContext,
        id: &EntryId,
        options: &EntryLoadOptions,
    ) -> ServiceResult<Entry>;

    async fn get_entries(
        &self,
        context: &RequestContext,
        ids: &[EntryId],
        options: &EntryLoadOptions,
    ) -> ServiceResult<Vec<Entry>>;

    async fn update_entry(
        &self,
        context: &RequestContext,
        request: EntryUpdateRequest,
    ) -> ServiceResult<()>;

    async fn delete_entry(&self, context: &RequestContext, id: &EntryId) -> ServiceResult<()>;

    async fn create_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentCreateRequest,
    ) -> ServiceResult<AttachmentId>;

    async fn update_attachment(
        &self,
        context: &RequestContext,
        request: AttachmentUpdateRequest,
    ) -> ServiceResult<()>;

    async fn delete_attachment(
        &self,
        context: &RequestContext,
        id: &AttachmentId,
    ) -> ServiceResult<()>;
}

// ============================================================
//  EntryLoadOption
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryLoadOptions {
    #[builder(default, setter(into))]
    pub load_content: bool,

    #[builder(default, setter(into))]
    pub load_attachments: bool,

    #[builder(default, setter(into))]
    pub load_times: bool,
}

// ============================================================
//  EntryCreateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug, Deserialize)]
pub struct EntryCreateRequest {
    #[builder(setter(into))]
    pub name: String,

    #[builder(default, setter(strip_option, into))]
    pub content_type: Option<String>,

    #[builder(default, setter(strip_option, into))]
    pub content: Option<String>,
}

// ============================================================
//  EntryUpdateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct EntryUpdateRequest {
    #[builder(setter(into))]
    pub id: EntryId,

    #[builder(default, setter(strip_option, into))]
    pub name: Option<String>,

    #[builder(default, setter(strip_option, into))]
    pub content_type: Option<String>,

    #[builder(default, setter(strip_option, into))]
    pub content: Option<String>,
}

// ============================================================
//  AttachmentCreateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct AttachmentCreateRequest {
    #[builder(setter(into))]
    pub entry_id: EntryId,

    #[builder(setter(into))]
    pub name: String,

    #[builder(setter(into))]
    pub file_path: PathBuf,
}

// ============================================================
//  AttachmentUpdateRequest
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct AttachmentUpdateRequest {
    #[builder(setter(into))]
    pub id: EntryId,

    #[builder(default, setter(strip_option, into))]
    pub entry_id: Option<EntryId>,

    #[builder(default, setter(strip_option, into))]
    pub name: Option<String>,
}
