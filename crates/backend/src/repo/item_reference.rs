use silver_brain_core::*;
use sqlx::{Acquire, Executor, Row, Sqlite, query, sqlite::SqliteRow};

use super::util::{self, ToServiceResponse};

pub(crate) async fn get<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &ItemReferenceId,
) -> ServiceResponse<Option<ItemReference>> {
    let sql = "select * from item_reference where id = ?";
    let query = query(sql).bind(id.as_str());

    util::acquire(conn)
        .await?
        .fetch_optional(query)
        .await
        .to_service_response()?
        .map(|it| to_item_reference(it))
        .transpose()
}

pub(crate) async fn get_all<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    item_id: &ItemId,
) -> ServiceResponse<Vec<ItemReference>> {
    let sql = "select * from item_reference where source = ? or target = ?";
    let query = query(sql).bind(item_id.as_str()).bind(item_id.as_str());

    util::acquire(conn)
        .await?
        .fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_item_reference)
        .collect()
}

pub(crate) async fn get_source<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    source: &ItemId,
) -> ServiceResponse<Vec<ItemReference>> {
    let sql = "select * from item_reference where source = ?";
    let query = query(sql).bind(source.as_str());

    util::acquire(conn)
        .await?
        .fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_item_reference)
        .collect()
}

pub(crate) async fn get_target<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    target: &ItemId,
) -> ServiceResponse<Vec<ItemReference>> {
    let sql = "select * from item_reference where target = ?";
    let query = query(sql).bind(target.as_str());

    util::acquire(conn)
        .await?
        .fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(to_item_reference)
        .collect()
}

pub(crate) async fn insert<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    reference: &ItemReference,
) -> ServiceResponse<()> {
    let sql = "insert into item_reference values(?, ?, ?, ?, ?, ?)";
    let query = query(sql)
        .bind(reference.id.as_str())
        .bind(reference.source.as_str())
        .bind(reference.target.as_str())
        .bind(&reference.annotation)
        .bind(reference.create_time)
        .bind(reference.update_time);

    util::acquire(conn)
        .await?
        .execute(query)
        .await
        .to_service_response()?;

    Ok(())
}

pub(crate) async fn update<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    reference: &ItemReference,
) -> ServiceResponse<()> {
    let sql =
        "update item_reference set annotation = ?, update_time = ? where source = ? and target = ?";

    let query = query(sql)
        .bind(reference.annotation.as_str())
        .bind(reference.update_time)
        .bind(reference.source.as_str())
        .bind(reference.target.as_str());

    util::acquire(conn)
        .await?
        .execute(query)
        .await
        .to_service_response()?;

    Ok(())
}

pub(crate) async fn delete<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &ItemReferenceId,
) -> ServiceResponse<()> {
    let sql = "delete from item_reference where id = ?";
    let query = query(sql).bind(id.as_str());

    util::acquire(conn)
        .await?
        .execute(query)
        .await
        .to_service_response()?;

    Ok(())
}

fn to_item_reference(row: SqliteRow) -> ServiceResponse<ItemReference> {
    let id: String = row.try_get("id").to_service_response()?;
    let source: String = row.try_get("source").to_service_response()?;
    let target: String = row.try_get("target").to_service_response()?;

    Ok(ItemReference {
        id: id.parse()?,
        source: source.parse()?,
        target: target.parse()?,
        annotation: row.try_get("annotation").to_service_response()?,
        create_time: row.try_get("create_time").to_service_response()?,
        update_time: row.try_get("update_time").to_service_response()?,
    })
}
