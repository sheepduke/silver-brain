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
                    .table(ParentLink::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ParentLink::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(ParentLink::Source).string().not_null())
                    .col(ColumnDef::new(ParentLink::Target).string().not_null())
                    .col(ColumnDef::new(ParentLink::Label).text().not_null())
                    .col(ColumnDef::new(ParentLink::CreateTime).string().not_null())
                    .col(ColumnDef::new(ParentLink::UpdateTime).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_source")
                            .from(ParentLink::Table, ParentLink::Source)
                            .to(Entry::Table, Entry::Id),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_target")
                            .from(ParentLink::Table, ParentLink::Target)
                            .to(Entry::Table, Entry::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ParentLink::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum ParentLink {
    Table,
    Id,
    Source,
    Target,
    Label,
    CreateTime,
    UpdateTime,
}
