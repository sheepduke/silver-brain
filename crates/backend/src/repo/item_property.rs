use silver_brain_core::*;
use sqlx::{Executor, Row, SqliteConnection, query, sqlite::SqliteRow};
use time::OffsetDateTime;

use super::util::ToServiceResponse;

pub(crate) async fn exists(
    conn: &mut SqliteConnection,
    item_id: &ItemId,
    key: &str,
) -> ServiceResponse<bool> {
    let sql = "select count(*) from item_property where item_id = ? and key = ?";

    let query = query(sql).bind(item_id.as_str()).bind(key);

    let count: i32 = conn.fetch_one(query).await.to_service_response()?.get(0);

    Ok(count > 0)
}

pub(crate) async fn get_all(
    conn: &mut SqliteConnection,
    item_id: &ItemId,
) -> ServiceResponse<Vec<ItemProperty>> {
    let sql = "select * from item_property where item_id = ?";
    let query = query(sql).bind(item_id.as_str());

    conn.fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_item_property)
        .collect()
}

pub(crate) async fn insert(
    conn: &mut SqliteConnection,
    item_id: &ItemId,
    property: &ItemProperty,
) -> ServiceResponse<()> {
    let sql = "insert into item_property values(?, ?, ?, ?, ?)";
    let time = OffsetDateTime::now_utc();
    let query = query(sql)
        .bind(item_id.as_str())
        .bind(&property.key)
        .bind(&property.value)
        .bind(time)
        .bind(time);

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

pub(crate) async fn update(
    conn: &mut SqliteConnection,
    item_id: &ItemId,
    ItemProperty { key, value }: &ItemProperty,
) -> ServiceResponse<()> {
    let sql = "update item_property set value = ?, update_time = ? where item_id = ? and key = ?";

    let query = query(sql)
        .bind(value)
        .bind(OffsetDateTime::now_utc())
        .bind(item_id.as_str())
        .bind(key);

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

pub(crate) async fn delete(
    conn: &mut SqliteConnection,
    item_id: &ItemId,
    key: &str,
) -> ServiceResponse<()> {
    let sql = "delete from item_property where item_id = ? and key = ?";
    let query = query(sql).bind(item_id.as_str()).bind(key);

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

fn to_item_property(row: SqliteRow) -> ServiceResponse<ItemProperty> {
    let key: String = row.try_get("key").to_service_response()?;
    let value: String = row.try_get("value").to_service_response()?;

    Ok(ItemProperty { key, value })
}
