use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Concept::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Concept::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Concept::Name).string().not_null())
                    .col(ColumnDef::new(Concept::ContentType).string().not_null())
                    .col(ColumnDef::new(Concept::Content).text().not_null())
                    .col(ColumnDef::new(Concept::CreateTime).string().not_null())
                    .col(ColumnDef::new(Concept::UpdateTime).string().not_null())
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Concept::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum Concept {
    Table,
    Id,
    Name,
    ContentType,
    Content,
    CreateTime,
    UpdateTime,
}
