pub use sea_orm_migration::prelude::*;
use sea_orm_migration::sea_orm::DatabaseConnection;

mod m20230920_010000_create_entry_table;
mod m20230920_233000_create_attachment_table;
mod m20231023_000500_create_parent_link_table;
mod m20231023_000700_create_friend_link_table;

pub struct Migrator;

impl Migrator {
    pub async fn run(db: &DatabaseConnection) -> Result<(), DbErr> {
        Ok(Migrator::refresh(db).await?)
    }
}

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20230920_010000_create_entry_table::Migration),
            Box::new(m20230920_233000_create_attachment_table::Migration),
            Box::new(m20231023_000500_create_parent_link_table::Migration),
            Box::new(m20231023_000700_create_friend_link_table::Migration),
        ]
    }
}
