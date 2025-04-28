use silver_brain_core::*;

use anyhow::Context;
use sqlx::{
    Acquire, Executor, QueryBuilder, Row, Sqlite, SqliteConnection, query, sqlite::SqliteRow,
};
use time::OffsetDateTime;

pub async fn exists<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &ItemId,
) -> ServiceResponse<bool> {
    let sql = "select count(*) from item where id = ?";

    let query = query(sql).bind(id.as_str());

    let count: i32 = conn
        .acquire()
        .await
        .context("")?
        .fetch_one(query)
        .await
        .context("Item exists db error")?
        .get(0);

    Ok(count > 0)
}

pub async fn get<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &ItemId,
    options: &ItemLoadOptions,
) -> ServiceResponse<Option<Item>> {
    let mut builder = QueryBuilder::new("select id, name");

    if options.load_content_type {
        builder.push(", content_type");
    }

    if options.load_content {
        builder.push(", content");
    }

    if options.load_create_time {
        builder.push(", create_time");
    }

    if options.load_update_time {
        builder.push(", update_time");
    }

    builder
        .push(" from item where id = ")
        .push_bind(id.as_str());

    let mut conn = conn.acquire().await.context("")?;

    let row = conn
        .fetch_optional(builder.build())
        .await
        .context("Get item db error")?;

    row.map(|it| row_to_item(it, options)).transpose()
}

pub async fn create<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &ItemId,
    name: &str,
    content_type: &str,
    content: &str,
) -> ServiceResponse<()> {
    let current_time = OffsetDateTime::now_utc();

    let mut conn = conn.acquire().await.context("")?;

    let query = query("insert into item values(?, ?, ?, ?, ?, ?)")
        .bind(id.as_str())
        .bind(name)
        .bind(content_type)
        .bind(content)
        .bind(current_time)
        .bind(current_time);

    conn.execute(query).await.context("Create item db error")?;

    Ok(())
}

pub async fn update<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &ItemId,
    name: Option<&str>,
    content_type: Option<&str>,
    content: Option<&str>,
) -> ServiceResponse<()> {
    let current_time = OffsetDateTime::now_utc();

    let mut builder = QueryBuilder::<Sqlite>::new("update item set update_time = ");
    builder.push_bind(current_time);

    if let Some(name) = name {
        builder.push(", name = ").push_bind(name);
    }

    if let Some(content_type) = content_type {
        builder.push(", content_type = ").push_bind(content_type);
    }

    if let Some(content) = content {
        builder.push(", content = ").push_bind(content);
    }

    builder.push(" where id = ").push_bind(id.as_str());

    conn.acquire()
        .await
        .context("")?
        .execute(builder.build())
        .await
        .context("Update item db error")?;

    Ok(())
}

pub async fn delete(conn: &mut SqliteConnection, id: &ItemId) -> ServiceResponse<()> {
    let sql = "delete from item where id = ?";
    query(sql)
        .bind(id.as_str())
        .execute(conn)
        .await
        .context("Delete item db error")?;

    Ok(())
}

fn row_to_item(row: SqliteRow, options: &ItemLoadOptions) -> ServiceResponse<Item> {
    let id: String = row.try_get("id").context("Invalid id")?;
    let name: String = row.try_get("name").context("Invalid name")?;

    let mut item = Item::new(ItemId::try_from(id)?, name);

    if options.load_content_type {
        item.content_type = row
            .try_get("content_type")
            .context("Invalid content_type")?;
    }

    if options.load_content {
        item.content = Some(row.try_get("content").context("Invalid content")?);
    }

    if options.load_create_time {
        item.create_time = Some(row.try_get("create_time").context("Invalid create_time")?);
    }

    if options.load_update_time {
        item.update_time = Some(row.try_get("update_time").context("Invalid update_time")?);
    }

    Ok(item)
}
