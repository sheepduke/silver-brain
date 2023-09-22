#[cfg(test)]
pub mod store {
    use std::fs::{self};

    use std::env;
    use std::sync::Once;

    use lazy_static::lazy_static;
    use svix_ksuid::{Ksuid, KsuidLike};

    use crate::store::{SqliteStore, SqliteStoreOptions};

    impl Drop for SqliteStore {
        fn drop(&mut self) {
            fs::remove_dir_all(&self.data_path).unwrap()
        }
    }

    lazy_static! {
        static ref INIT: Once = Once::new();
    }

    pub fn new_sqlite_store() -> SqliteStore {
        INIT.call_once(|| {
            tracing_subscriber::fmt()
                .with_max_level(tracing::Level::DEBUG)
                .with_test_writer()
                .init();
        });

        let mut path = env::temp_dir();
        path.push("silver-brain-tests");
        path.push(Ksuid::new(None, None).to_string());

        SqliteStore::new(path, SqliteStoreOptions::default()).unwrap()
    }
}
