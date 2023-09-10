use std::marker::PhantomData;

use anyhow::Result;

use crate::{
    store::{DataSource, SqlStore, Store},
    Entry, EntryId,
};

#[derive(Default)]
pub struct EntryGraph<S, D>
where
    S: Store<D>,
    D: DataSource,
{
    store: S,
    _phantom: PhantomData<D>,
}

impl<S, D> EntryGraph<S, D>
where
    S: Store<D>,
    D: DataSource,
{
    pub fn new(store: S) -> EntryGraph<S, D> {
        Self {
            store,
            _phantom: Default::default(),
        }
    }

    pub async fn temp(&self) -> Result<()> {
        let conn = self.store.get_conn("some".into()).await?;

        let entry = Entry::default();

        Ok(())
    }
}
