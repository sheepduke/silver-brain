mod entry;
pub use entry::{
    AttachmentCreateRequest, AttachmentUpdateRequest, EntryCreateRequest, EntryLoadOptions,
    EntryService, EntryUpdateRequest,
};

mod error;
pub use error::ClientError;
