use silver_brain_core::*;
use sqlx::{QueryBuilder, Sqlite};

use crate::{
    DatabaseConnector,
    repo::{self},
};

use super::SqlService;

// ============================================================
//  Trait Impl
// ============================================================

impl<C> SearchService for SqlService<C>
where
    C: DatabaseConnector,
{
    async fn search(
        &self,
        context: &RequestContext,
        search: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Vec<Item>> {
        let query = search::parse(search)?;

        self.connector
            .with_transaction(&context.repo_name, async |tx| {
                let mut builder = QueryBuilder::new("");
                build_query(&mut builder, query)?;

                let ids = repo::item::get_ids(&mut *tx, &mut builder).await?;
                repo::item::get_many(&mut *tx, &ids, options).await
            })
            .await
    }
}

// ============================================================
//  Main
// ============================================================

fn build_query(builder: &mut QueryBuilder<Sqlite>, query: SearchQuery) -> ServiceResponse<()> {
    match query {
        SearchQuery::And(queries) => build_and_query(builder, queries),
        SearchQuery::Or(queries) => build_or_query(builder, queries),
        SearchQuery::Not(query) => build_not_query(builder, *query),
        SearchQuery::Keyword(keyword) => build_keyword_query(builder, keyword),
        SearchQuery::Property {
            key,
            operator,
            value,
        } => build_property_query(builder, key, operator, value),
        SearchQuery::Filter(filter_query) => build_filter_query(builder, filter_query),
    }
}

fn build_and_query(
    builder: &mut QueryBuilder<Sqlite>,
    queries: Vec<SearchQuery>,
) -> ServiceResponse<()> {
    if let Some((first, rest)) = queries.split_first() {
        builder.push("(");
        build_query(&mut (*builder), first.to_owned())?;

        for query in rest {
            builder.push(" INTERSECT ");
            build_query(&mut (*builder), query.to_owned())?;
        }

        builder.push(")");
    }

    Ok(())
}

fn build_or_query(
    builder: &mut QueryBuilder<Sqlite>,
    queries: Vec<SearchQuery>,
) -> ServiceResponse<()> {
    if let Some((first, rest)) = queries.split_first() {
        builder.push("(");
        build_query(&mut (*builder), first.to_owned())?;

        for query in rest {
            builder.push(" UNION ");
            build_query(&mut (*builder), query.to_owned())?;
        }

        builder.push(")");
    }

    Ok(())
}

fn build_not_query(builder: &mut QueryBuilder<Sqlite>, query: SearchQuery) -> ServiceResponse<()> {
    builder.push("select id from item where id not in (");
    build_query(builder, query)?;
    builder.push(")");

    Ok(())
}

fn build_keyword_query(builder: &mut QueryBuilder<Sqlite>, keyword: String) -> ServiceResponse<()> {
    build_filter_query(
        builder,
        FilterQuery::Name {
            operator: CompareOperator::Filter,
            value: keyword,
        },
    )?;

    Ok(())
}

fn build_property_query(
    builder: &mut QueryBuilder<Sqlite>,
    key: String,
    operator: CompareOperator,
    value: String,
) -> ServiceResponse<()> {
    builder
        .push("select item_id from item_property where key = ")
        .push_bind(key)
        .push(" and value ")
        .push(convert_compare_operator(operator))
        .push_bind(value);

    Ok(())
}

// ============================================================
//  Filter
// ============================================================

fn build_filter_query(
    builder: &mut QueryBuilder<Sqlite>,
    query: FilterQuery,
) -> ServiceResponse<()> {
    match query {
        FilterQuery::Name { operator, value } => build_filter_name_query(builder, operator, value)?,
        FilterQuery::ContentType { operator, value } => todo!(),
        FilterQuery::Content { operator, value } => todo!(),
        FilterQuery::HasProperty { value } => todo!(),
        FilterQuery::ParentsCount { operator, value } => todo!(),
        FilterQuery::ChildrenCount { operator, value } => todo!(),
    }

    Ok(())
}

fn build_filter_name_query(
    builder: &mut QueryBuilder<Sqlite>,
    operator: CompareOperator,
    value: String,
) -> ServiceResponse<()> {
    builder
        .push("select id from item where name")
        .push(convert_compare_operator(operator))
        .push_bind(convert_string_value(value, operator));

    Ok(())
}

fn convert_compare_operator(operator: CompareOperator) -> &'static str {
    match operator {
        CompareOperator::LessThan => " < ",
        CompareOperator::LessEqual => " <= ",
        CompareOperator::Filter | CompareOperator::Match => " like ",
        CompareOperator::Equal => " = ",
        CompareOperator::NotEqual => " <> ",
        CompareOperator::GreaterEqual => " >= ",
        CompareOperator::GreaterThan => " > ",
    }
}

fn convert_string_value(value: String, operator: CompareOperator) -> String {
    match operator {
        CompareOperator::Filter => format!("%{value}%"),
        CompareOperator::Match => value.replace("*", "%"),
        _ => value,
    }
}

// ============================================================
//  Tests
// ============================================================

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use anyhow::Result;
    use silver_brain_core::*;

    use crate::{InMemorySqliteConnector, SqlService};

    #[tokio::test]
    async fn test() -> Result<()> {
        let (service, context) = setup().await?;
        let options = ItemLoadOptions::core();

        let query = "name ~ *";
        let items = service.search(&context, query, &options).await?;

        println!("=== Items ===");

        for item in items {
            println!("Item [{}]: {}", item.id, item.name);
        }

        println!("=== END ===");

        panic!()
    }

    async fn setup() -> Result<(impl SearchService, RequestContext)> {
        let service = SqlService::new(Arc::new(InMemorySqliteConnector::new()?));
        let repo_name: RepoName = "main".parse()?;
        let context = RequestContext::builder().repo_name(repo_name).build();

        let request = CreateItemRequest::builder().name("Software").build();
        service.create_item(&context, request).await?;

        let request = CreateItemRequest::builder().name("Emacs").build();
        service.create_item(&context, request).await?;

        let request = CreateItemRequest::builder().name("Vim").build();
        service.create_item(&context, request).await?;

        Ok((service, context))
    }
}
