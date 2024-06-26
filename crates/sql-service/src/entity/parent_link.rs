//! `SeaORM` Entity. Generated by sea-orm-codegen 0.12.2

use sea_orm::entity::prelude::*;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, Eq)]
#[sea_orm(table_name = "parent_link")]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    pub id: String,
    pub source: String,
    pub target: String,
    pub label: String,
    pub create_time: String,
    pub update_time: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::entry::Entity",
        from = "Column::Target",
        to = "super::entry::Column::Id",
        on_update = "NoAction",
        on_delete = "NoAction"
    )]
    Entry2,
    #[sea_orm(
        belongs_to = "super::entry::Entity",
        from = "Column::Source",
        to = "super::entry::Column::Id",
        on_update = "NoAction",
        on_delete = "NoAction"
    )]
    Entry1,
}

impl ActiveModelBehavior for ActiveModel {}
