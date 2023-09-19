use sea_orm_migration::prelude::*;

use crate::m20230920_010000_create_concept_table::Concept;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(ConceptTag::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ConceptTag::Id)
                            .string()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(ConceptTag::ConceptId).string().not_null())
                    .col(ColumnDef::new(ConceptTag::Name).string().not_null())
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_concept_id")
                            .from(ConceptTag::Table, ConceptTag::ConceptId)
                            .to(Concept::Table, Concept::Id),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ConceptTag::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
pub enum ConceptTag {
    Table,
    Id,
    ConceptId,
    Name,
}
