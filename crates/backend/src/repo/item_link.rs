use sqlx::{Acquire, Executor, Row, Sqlite, query};

use silver_brain_core::*;

use crate::repo::util::ToServiceResponse;

use super::util;

pub async fn exists<'a>(
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

    Ok(count > 0)
}
