use std::str::FromStr;

use silver_brain_core::*;
use sqlx::{Executor, SqliteConnection, prelude::FromRow, query, query_as};
use time::OffsetDateTime;

use super::{item::CoreItemRow, util::ToServiceResponse};

pub(crate) async fn get(
    conn: &mut SqliteConnection,
    id: &ItemReferenceId,
) -> ServiceResponse<Option<ItemReference>> {
    let sql = "select * from item_reference where id = ?";

    let row = query_as::<_, ItemReferenceRow>(sql)
        .bind(id.as_str())
        .fetch_optional(&mut *conn)
        .await
        .to_service_response()?;

    match row {
        Some(row) => Ok(Some(to_item_reference(conn, row).await?)),

        None => Ok(None),
    }
}

pub(crate) async fn get_source(
    conn: &mut SqliteConnection,
    source: &ItemId,
) -> ServiceResponse<Vec<ItemReference>> {
    let sql = "select * from item_reference where source = ?";

    let rows = query_as::<_, ItemReferenceRow>(sql)
        .bind(source.as_str())
        .fetch_all(&mut *conn)
        .await
        .to_service_response()?;

    let mut result = Vec::new();

    for row in rows {
        result.push(to_item_reference(&mut *conn, row).await?);
    }

    Ok(result)
}

pub(crate) async fn get_target(
    conn: &mut SqliteConnection,
    target: &ItemId,
) -> ServiceResponse<Vec<ItemReference>> {
    let sql = "select * from item_reference where target = ?";

    let rows = query_as::<_, ItemReferenceRow>(sql)
        .bind(target.as_str())
        .fetch_all(&mut *conn)
        .await
        .to_service_response()?;

    let mut result = Vec::new();

    for row in rows {
        result.push(to_item_reference(&mut *conn, row).await?);
    }

    Ok(result)
}

pub(crate) async fn insert(
    conn: &mut SqliteConnection,
    reference: &ItemReference,
) -> ServiceResponse<()> {
    let sql = "insert into item_reference values(?, ?, ?, ?, ?, ?)";
    let query = query(sql)
        .bind(reference.id.as_str())
        .bind(reference.source.id.as_str())
        .bind(reference.target.id.as_str())
        .bind(&reference.annotation)
        .bind(reference.create_time)
        .bind(reference.update_time);

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

pub(crate) async fn update(
    conn: &mut SqliteConnection,
    reference: &ItemReference,
) -> ServiceResponse<()> {
    let sql =
        "update item_reference set annotation = ?, update_time = ? where source = ? and target = ?";

    let query = query(sql)
        .bind(reference.annotation.as_str())
        .bind(reference.update_time)
        .bind(reference.source.id.as_str())
        .bind(reference.target.id.as_str());

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

pub(crate) async fn delete(
    conn: &mut SqliteConnection,
    id: &ItemReferenceId,
) -> ServiceResponse<()> {
    let sql = "delete from item_reference where id = ?";
    let query = query(sql).bind(id.as_str());

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

async fn get_core_item(conn: &mut SqliteConnection, id: &str) -> ServiceResponse<CoreItem> {
    let sql = "select id, name from item where id = ?";

    let row = query_as::<_, CoreItemRow>(sql)
        .bind(id)
        .fetch_optional(conn)
        .await
        .to_service_response()?
        .ok_or(ServiceError::DataCorrupted(format!(
            "Item `{}` not found",
            id
        )))?;

    Ok(CoreItem::builder()
        .id(ItemId::from_str(&row.id)?)
        .name(row.name)
        .build())
}

#[derive(FromRow)]
pub(crate) struct ItemReferenceRow {
    pub id: String,
    pub source: String,
    pub target: String,
    pub annotation: String,
    pub create_time: OffsetDateTime,
    pub update_time: OffsetDateTime,
}

async fn to_item_reference(
    conn: &mut SqliteConnection,
    row: ItemReferenceRow,
) -> ServiceResponse<ItemReference> {
    let id = ItemReferenceId::from_str(&row.id)?;
    let source = get_core_item(conn, &row.source).await?;
    let target = get_core_item(conn, &row.target).await?;

    Ok(ItemReference::builder()
        .id(id)
        .source(source)
        .target(target)
        .annotation(row.annotation)
        .create_time(row.create_time)
        .update_time(row.update_time)
        .build())
}
