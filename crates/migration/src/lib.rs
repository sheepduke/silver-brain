pub use sea_orm_migration::prelude::*;

mod m20230920_010000_create_concept_table;
mod m20230920_010100_create_concept_tag_table;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20230920_010000_create_concept_table::Migration),
            Box::new(m20230920_010100_create_concept_tag_table::Migration),
        ]
    }
}
