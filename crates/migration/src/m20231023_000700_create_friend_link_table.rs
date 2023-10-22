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
                    .table(FriendLink::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(FriendLink::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(FriendLink::Source).string().not_null())
                    .col(ColumnDef::new(FriendLink::Target).string().not_null())
                    .col(ColumnDef::new(FriendLink::Label).text().not_null())
                    .col(ColumnDef::new(FriendLink::IsMutual).boolean().not_null())
                    .col(ColumnDef::new(FriendLink::CreateTime).string().not_null())
                    .col(ColumnDef::new(FriendLink::UpdateTime).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_source")
                            .from(FriendLink::Table, FriendLink::Source)
                            .to(Entry::Table, Entry::Id),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_target")
                            .from(FriendLink::Table, FriendLink::Target)
                            .to(Entry::Table, Entry::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(FriendLink::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum FriendLink {
    Table,
    Id,
    Source,
    Target,
    Label,
    IsMutual,
    CreateTime,
    UpdateTime,
}
