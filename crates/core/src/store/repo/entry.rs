use anyhow::Result;
use store::StoreConnection;

use crate::{store::store, Entry, EntryId};

pub struct SaveRequest {
    name: Option<String>,
    content_type: Option<String>,
    content: Option<String>,
}

fn create((store_conn): StoreConnection, request: SaveRequest) -> Result<EntryId> {
    todo!()
}

async fn get((store_conn): StoreConnection, id: EntryId) -> Result<Entry> {
    todo!()
}

async fn update((store_conn): StoreConnection, id: EntryId, request: SaveRequest) -> Result<()> {
    todo!()
}

async fn delete((store_conn): StoreConnection, id: EntryId) -> Result<()> {
    todo!()
}
