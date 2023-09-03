use anyhow::Result;

use crate::{
    store::{SqlStore, Store},
    Entry,
};

#[derive(Default)]
pub struct EntryGraph<StoreT: Store = SqlStore> {
    store: StoreT,
}

impl<T: Store> EntryGraph<T> {
    pub fn new(store: T) -> EntryGraph<T> {
        Self { store }
    }

    pub async fn temp(&self) -> Result<()> {
        let conn = self.store.get_connection("some".into()).await?;

        let entry = Entry::default();

        Ok(())
    }
}

impl EntryGraph {
    pub fn default() -> EntryGraph<SqlStore> {
        Default::default()
    }
}
