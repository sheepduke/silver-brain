pub use sea_orm_migration::prelude::*;

mod m20230920_010000_create_entry_table;
mod m20230920_010100_create_entry_tag_table;
mod m20230920_233000_create_attachment_table;
mod m20230920_233300_create_link_table;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20230920_010000_create_entry_table::Migration),
            Box::new(m20230920_010100_create_entry_tag_table::Migration),
            Box::new(m20230920_233000_create_attachment_table::Migration),
            Box::new(m20230920_233300_create_link_table::Migration),
        ]
    }
}
