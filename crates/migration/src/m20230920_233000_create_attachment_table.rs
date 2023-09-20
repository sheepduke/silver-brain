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
                    .table(Attachment::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Attachment::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Attachment::EntryId).string().not_null())
                    .col(ColumnDef::new(Attachment::Name).text().not_null())
                    .col(ColumnDef::new(Attachment::FilePath).string().not_null())
                    .col(ColumnDef::new(Attachment::Size).integer().not_null())
                    .col(ColumnDef::new(Attachment::CreateTime).string().not_null())
                    .col(ColumnDef::new(Attachment::UpdateTime).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_entry_id")
                            .from(Attachment::Table, Attachment::EntryId)
                            .to(Entry::Table, Entry::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Attachment::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum Attachment {
    Table,
    Id,
    EntryId,
    Name,
    FilePath,
    Size,
    CreateTime,
    UpdateTime,
}
