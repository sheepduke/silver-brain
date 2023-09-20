use sea_orm_migration::prelude::*;

use crate::m20230920_010000_create_entry_table::Entry;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(EntryTag::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(EntryTag::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(EntryTag::EntryId).string().not_null())
                    .col(ColumnDef::new(EntryTag::Name).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_entry_id")
                            .from(EntryTag::Table, EntryTag::EntryId)
                            .to(Entry::Table, Entry::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(EntryTag::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum EntryTag {
    Table,
    Id,
    EntryId,
    Name,
}
