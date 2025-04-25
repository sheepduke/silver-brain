use silver_brain_core::*;

use anyhow::Context;
use sqlx::{Executor, FromRow, Row, Sqlite, query, query_as, sqlite::SqliteRow};
use time::OffsetDateTime;

pub async fn create<'a, E>(executor: E, item: Item) -> ServiceResponse<()>
where
    E: Executor<'a, Database = Sqlite>,
{
    todo!()
}

pub async fn get<'a, E>(
    conn: E,
    id: ItemId,
    options: ItemLoadOptions,
) -> ServiceResponse<Option<Item>>
where
    E: Executor<'a, Database = Sqlite>,
{
    let mut selects = "id, name".to_string();

    if options.load_content_type {
        selects.push_str(", content_type");
    }

    if options.load_content {
        selects.push_str(", content");
    }

    if options.load_create_time {
        selects.push_str(", create_time");
    }

    if options.load_update_time {
        selects.push_str(", update_time");
    }

    let sql = format!("select {selects} from item where id = ?");

    let row = query(&sql)
        .bind(id.as_str())
        .fetch_optional(conn)
        .await
        .context("Database error")?;

    row.map(|it| row_to_item(it, options)).transpose()
}

pub async fn update(item: Item) -> ServiceResponse<()> {
    todo!()
}

pub async fn delete(id: ItemId) -> ServiceResponse<()> {
    todo!()
}

fn row_to_item(row: SqliteRow, options: ItemLoadOptions) -> ServiceResponse<Item> {
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

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use silver_brain_core::{ItemId, ItemLoadOptions};

    use anyhow::Result;
    use sqlx::{Connection, SqliteConnection, SqlitePool};

    use tokio;

    #[tokio::test]
    async fn asdf() -> Result<()> {
        let url = "sqlite:///home/sheep/temp/test/main/data.sqlite";

        let pool = SqlitePool::connect_lazy(url)?;

        let item_opt = super::get(
            &pool,
            ItemId::from_str("i_1KED0QeRu1ph4t0nAqJZ2HYBREI")?,
            ItemLoadOptions {
                load_create_time: true,
                ..ItemLoadOptions::default()
            },
        )
        .await;

        println!("Item: {:?}", item_opt);

        panic!();
    }
}
