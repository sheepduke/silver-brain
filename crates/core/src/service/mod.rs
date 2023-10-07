mod entry;
pub use entry::{
    AttachmentCreateRequest, AttachmentUpdateRequest, EntryCreateRequest, EntryLoadOptions,
    EntryService, EntryTagCreateRequest, EntryTagUpdateRequest, EntryUpdateRequest,
};

mod error;
pub use error::ClientError;
