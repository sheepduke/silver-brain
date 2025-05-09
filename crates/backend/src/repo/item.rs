use crate::repo::util::ToServiceResponse;
use silver_brain_core::*;

use sqlx::{
    Acquire, Execute, Executor, QueryBuilder, Row, Sqlite, SqliteConnection, query,
    sqlite::SqliteRow,
};
use time::OffsetDateTime;

use super::util;

pub(crate) async fn get<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    id: &'a ItemId,
    options: &'a ItemLoadOptions,
) -> ServiceResponse<Option<Item>> {
    let mut builder = QueryBuilder::new("");
    build_select_query(&mut builder, options);
    builder.push(" where id = ").push_bind(id.as_str());

    util::acquire(conn)
        .await?
        .fetch_optional(builder.build())
        .await
        .to_service_response()?
        .map(|it| row_to_item(it, options))
        .transpose()
}

pub(crate) async fn get_many<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    ids: &[ItemId],
    options: &'a ItemLoadOptions,
) -> ServiceResponse<Vec<Item>> {
    if ids.is_empty() {
        Ok(Vec::new())
    } else {
        let mut builder = QueryBuilder::new("");
        build_select_query(&mut builder, options);
        builder.push(" where id in (");

        if let Some((first, rest)) = ids.split_first() {
            builder.push_bind(first.as_str());

            for id in rest {
                builder.push(",").push_bind(id.as_str());
            }
        }

        builder.push(")");

        util::acquire(conn)
            .await?
            .fetch_all(builder.build())
            .await
            .to_service_response()?
            .into_iter()
            .map(|it| row_to_item(it, options))
            .collect()
    }
}

pub(crate) async fn get_ids<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    builder: &mut QueryBuilder<'a, Sqlite>,
) -> ServiceResponse<Vec<ItemId>> {
    let query = builder.build();

    println!("SQL: ");
    println!("{:?}", query.sql());

    util::acquire(conn)
        .await?
        .fetch_all(query)
        .await
        .to_service_response()?
        .into_iter()
        .map(|row: SqliteRow| {
            let id: String = row.try_get(0).to_service_response()?;
            id.parse()
        })
        .collect()
}

pub(crate) async fn insert<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    item: &Item,
) -> ServiceResponse<()> {
    let current_time = OffsetDateTime::now_utc();

    let mut conn = util::acquire(conn).await?;

    let query = query("insert into item values(?, ?, ?, ?, ?, ?)")
        .bind(item.id.as_str())
        .bind(item.name.as_str())
        .bind(item.content_type.as_deref().unwrap_or(""))
        .bind(item.content.as_deref().unwrap_or(""))
        .bind(item.create_time.unwrap_or(current_time))
        .bind(item.update_time.unwrap_or(current_time));

    conn.execute(query).await.to_service_response()?;

    Ok(())
}

pub(crate) async fn update<'a>(
    conn: impl Acquire<'a, Database = Sqlite>,
    item: &Item,
) -> ServiceResponse<()> {
    let current_time = OffsetDateTime::now_utc();

    let mut builder = QueryBuilder::<Sqlite>::new("update item set update_time = ");

    builder
        .push_bind(item.update_time.unwrap_or(current_time))
        .push(", name = ")
        .push_bind(item.name.as_str());

    if let Some(content_type) = &item.content_type {
        builder.push(", content_type = ").push_bind(content_type);
    }

    if let Some(content) = &item.content {
        builder.push(", content = ").push_bind(content);
    }

    builder.push(" where id = ").push_bind(item.id.as_str());

    util::acquire(conn)
        .await?
        .execute(builder.build())
        .await
        .to_service_response()?;

    Ok(())
}

pub(crate) async fn delete(conn: &mut SqliteConnection, id: &ItemId) -> ServiceResponse<()> {
    let sql = "delete from item where id = ?";
    let query = query(sql).bind(id.as_str());

    util::acquire(conn)
        .await?
        .execute(query)
        .await
        .to_service_response()?;

    Ok(())
}

fn build_select_query(builder: &mut QueryBuilder<Sqlite>, options: &ItemLoadOptions) {
    builder.push("select id, name");

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

    builder.push(" from item");
}

fn row_to_item(row: SqliteRow, options: &ItemLoadOptions) -> ServiceResponse<Item> {
    let id: String = row.try_get("id").to_service_response()?;
    let item_id: ItemId = id.parse()?;
    let name: String = row.try_get("name").to_service_response()?;

    let mut item = Item::builder().id(item_id).name(name).build();

    if options.load_content_type {
        item.content_type = row.try_get("content_type").to_service_response()?;
    }

    if options.load_content {
        item.content = Some(row.try_get("content").to_service_response()?);
    }

    if options.load_create_time {
        item.create_time = Some(row.try_get("create_time").to_service_response()?);
    }

    if options.load_update_time {
        item.update_time = Some(row.try_get("update_time").to_service_response()?);
    }

    Ok(item)
}
