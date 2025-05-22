use sqlx::{Executor, Row, SqliteConnection, query, sqlite::SqliteRow};

use silver_brain_core::*;
use time::OffsetDateTime;

use crate::repo::util::ToServiceResponse;

pub(crate) async fn exists(
    conn: &mut SqliteConnection,
    parent: &ItemId,
    child: &ItemId,
) -> ServiceResponse<bool> {
    let sql = "select count(*) from item_link where parent = ? and child = ?";
    let query = query(sql).bind(parent.as_str()).bind(child.as_str());

    let count: i32 = conn.fetch_one(query).await.to_service_response()?.get(0);

    Ok(count > 0)
}

pub(crate) async fn get_parents(
    conn: &mut SqliteConnection,
    child: &ItemId,
) -> ServiceResponse<Vec<CoreItem>> {
    let sql =
        "select id, name from item where id in (select parent from item_link where child = ?)";

    let query = query(sql).bind(child.as_str());

    conn.fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_core_item)
        .collect()
}

pub(crate) async fn get_children(
    conn: &mut SqliteConnection,
    parent: &ItemId,
) -> ServiceResponse<Vec<CoreItem>> {
    let sql =
        "select id, name from item where id in (select child from item_link where parent = ?)";

    let query = query(sql).bind(parent.as_str());

    conn.fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_core_item)
        .collect()
}

pub(crate) async fn insert(
    conn: &mut SqliteConnection,
    parent: &ItemId,
    child: &ItemId,
) -> ServiceResponse<()> {
    let sql = "insert into item_link values(?, ?, ?)";
    let query = query(sql)
        .bind(parent.as_str())
        .bind(child.as_str())
        .bind(OffsetDateTime::now_utc());

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

pub(crate) async fn delete(
    conn: &mut SqliteConnection,
    parent: &ItemId,
    child: &ItemId,
) -> ServiceResponse<()> {
    let sql = "delete from item_link where parent = ? and child = ?";
    let query = query(sql).bind(parent.as_str()).bind(child.as_str());

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

fn to_core_item(row: SqliteRow) -> ServiceResponse<CoreItem> {
    let id: String = row.try_get("id").to_service_response()?;
    let item_id: ItemId = id.parse()?;

    Ok(CoreItem {
        id: item_id,
        name: row.try_get("name").to_service_response()?,
    })
}
