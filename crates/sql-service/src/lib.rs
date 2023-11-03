//! SQL based service implementation.

#![warn(missing_docs)]
//#![deny(warnings)]
#![deny(clippy::pedantic)]
#![allow(clippy::doc_markdown)]
#![allow(clippy::implicit_hasher)]
#![allow(clippy::manual_let_else)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::module_inception)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::new_without_default)]
#![allow(clippy::struct_excessive_bools)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::uninlined_format_args)]

/// Automatically generated entity.
mod entity;

/// External functions for entity.
mod entity_ext;

mod sqlite_store;
pub use sqlite_store::{SqliteStore, SqliteStoreOptions, StoreError};

/// Service implementation.
mod service;
pub use service::entry::SqlEntryService;
