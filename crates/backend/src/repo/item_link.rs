use sqlx::{Acquire, Execute, Executor, Row, Sqlite, query, sqlite::SqliteRow};

use silver_brain_core::*;
use time::OffsetDateTime;

use crate::repo::util::ToServiceResponse;

use super::util;

pub(crate) async fn exists<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    parent: &'a ItemId,
    child: &'a ItemId,
) -> ServiceResponse<bool> {
    let sql = "select count(*) from item_link where parent = ? and child = ?";
    let query = query(sql).bind(parent.as_str()).bind(child.as_str());

    let count: i32 = util::acquire(conn)
        .await?
        .fetch_one(query)
        .await
        .to_service_response()?
        .get(0);

    println!("Count = {}", count);

    Ok(count > 0)
}

pub(crate) async fn get_parents<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    child: &ItemId,
) -> ServiceResponse<Vec<CoreItem>> {
    let sql =
        "select id, name from item where id in (select parent from item_link where child = ?)";

    let query = query(sql).bind(child.as_str());

    util::acquire(conn)
        .await?
        .fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_core_item)
        .collect()
}

pub(crate) async fn get_children<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    parent: &ItemId,
) -> ServiceResponse<Vec<CoreItem>> {
    let sql =
        "select id, name from item where id in (select child from item_link where parent = ?)";

    let query = query(sql).bind(parent.as_str());

    util::acquire(conn)
        .await?
        .fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_core_item)
        .collect()
}

pub(crate) async fn insert<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    parent: &ItemId,
    child: &ItemId,
) -> ServiceResponse<()> {
    let sql = "insert into item_link values(?, ?, ?)";
    let query = query(sql)
        .bind(parent.as_str())
        .bind(child.as_str())
        .bind(OffsetDateTime::now_utc());

    util::acquire(conn)
        .await?
        .execute(query)
        .await
        .to_service_response()?;

    Ok(())
}

pub(crate) async fn delete<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    parent: &ItemId,
    child: &ItemId,
) -> ServiceResponse<()> {
    let sql = "delete from item_link where parent = ? and child = ?";
    let query = query(sql).bind(parent.as_str()).bind(child.as_str());

    util::acquire(conn)
        .await?
        .execute(query)
        .await
        .to_service_response()?;

    Ok(())
}

fn to_core_item(row: SqliteRow) -> ServiceResponse<CoreItem> {
    let id: String = row.try_get("id").to_service_response()?;

    Ok(CoreItem {
        id: ItemId::try_from(id)?,
        name: row.try_get("name").to_service_response()?,
    })
}
