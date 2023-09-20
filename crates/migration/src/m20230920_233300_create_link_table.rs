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
                    .table(Link::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Link::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(Link::Source).string().not_null())
                    .col(ColumnDef::new(Link::Target).string().not_null())
                    .col(ColumnDef::new(Link::Annotation).text().not_null())
                    .col(ColumnDef::new(Link::CreateTime).string().not_null())
                    .col(ColumnDef::new(Link::UpdateTime).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_source")
                            .from(Link::Table, Link::Source)
                            .to(Entry::Table, Entry::Id),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_target")
                            .from(Link::Table, Link::Target)
                            .to(Entry::Table, Entry::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Link::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum Link {
    Table,
    Id,
    Source,
    Target,
    Annotation,
    CreateTime,
    UpdateTime,
}
