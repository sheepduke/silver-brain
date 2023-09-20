use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Entry::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Entry::Id).string().not_null().primary_key())
                    .col(ColumnDef::new(Entry::Name).string().not_null())
                    .col(ColumnDef::new(Entry::ContentType).string().not_null())
                    .col(ColumnDef::new(Entry::Content).text().not_null())
                    .col(ColumnDef::new(Entry::CreateTime).string().not_null())
                    .col(ColumnDef::new(Entry::UpdateTime).string().not_null())
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Entry::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum Entry {
    Table,
    Id,
    Name,
    ContentType,
    Content,
    CreateTime,
    UpdateTime,
}
