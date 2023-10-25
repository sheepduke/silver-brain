mod entry;
pub use entry::{
    AttachmentCreateRequest, AttachmentUpdateRequest, EntryCreateRequest, EntryLoadOptions,
    EntryService, EntryUpdateRequest,
};

mod result;
pub use result::ServiceClientError;
