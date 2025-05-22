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

impl<C> ItemSearchService for SqlService<C>
where
    C: DatabaseConnector + Send + Sync,
{
    async fn search_items(
        &self,
        context: &RequestContext,
        search: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Vec<Item>> {
        let query = search::parse(search)?;

        let mut conn = self.connector.get_connection(&context.repo_name).await?;

        let mut builder = QueryBuilder::new("");
        build_query(&mut builder, query)?;

        let ids = repo::item::get_ids(&mut conn, &mut builder).await?;
        repo::item::get_many(&mut conn, &ids, options).await
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
        .push_bind(convert_string_value(value, operator));

    Ok(())
}

// ============================================================
//  Filter
// ============================================================

#[expect(unused)]
fn build_filter_query(
    builder: &mut QueryBuilder<Sqlite>,
    query: FilterQuery,
) -> ServiceResponse<()> {
    match query {
        FilterQuery::Name { operator, value } => build_filter_name_query(builder, operator, value)?,
        FilterQuery::ContentType { operator, value } => {
            build_filter_content_type_query(builder, operator, value)?
        }
        FilterQuery::Content { operator, value } => {
            build_filter_content_query(builder, operator, value)?
        }
        FilterQuery::HasProperty { operator, value } => {
            build_has_property_query(builder, operator, value)?
        }
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

fn build_filter_content_type_query(
    builder: &mut QueryBuilder<Sqlite>,
    operator: CompareOperator,
    value: String,
) -> ServiceResponse<()> {
    builder
        .push("select id from item where content_type")
        .push(convert_compare_operator(operator))
        .push_bind(convert_string_value(value, operator));

    Ok(())
}

fn build_filter_content_query(
    builder: &mut QueryBuilder<Sqlite>,
    operator: CompareOperator,
    value: String,
) -> ServiceResponse<()> {
    builder
        .push("select id from item where content")
        .push(convert_compare_operator(operator))
        .push_bind(convert_string_value(value, operator));

    Ok(())
}

fn build_has_property_query(
    builder: &mut QueryBuilder<Sqlite>,
    operator: CompareOperator,
    value: String,
) -> ServiceResponse<()> {
    builder
        .push("select item_id from item_property where key")
        .push(convert_compare_operator(operator))
        .push_bind(convert_string_value(value, operator));

    Ok(())
}

fn convert_compare_operator(operator: CompareOperator) -> String {
    let str = match operator {
        CompareOperator::LessThan => "<",
        CompareOperator::LessEqual => "<=",
        CompareOperator::Match | CompareOperator::Filter => "like",
        CompareOperator::Equal => "=",
        CompareOperator::NotEqual => "<>",
        CompareOperator::GreaterEqual => ">=",
        CompareOperator::GreaterThan => ">",
    };

    format!(" {} ", str)
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
    use anyhow::Result;
    use silver_brain_core::*;

    use crate::item::util::tests::setup;

    #[tokio::test]
    async fn search_has() -> Result<()> {
        assert_eq!(search("has: type").await?, vec!["Emacs", "Firefox", "Vim"]);
        assert_eq!(search("has~T*").await?, vec!["Emacs", "Firefox", "Vim"]);
        assert_eq!(search("has: xx").await?, Vec::<String>::new());
        assert_eq!(search("!has: TYPE").await?, vec!["Software"]);

        Ok(())
    }

    async fn search(search: &str) -> Result<Vec<String>> {
        let (service, context, _) = setup().await?;

        let mut item_names: Vec<String> = service
            .search_items(&context, search, &ItemLoadOptions::core())
            .await?
            .into_iter()
            .map(|it| it.name)
            .collect();

        item_names.sort();

        Ok(item_names)
    }
}
