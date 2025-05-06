use anyhow::Context;
use silver_brain_core::ServiceResponse;
use sqlx::{Acquire, Sqlite};

pub async fn acquire<'a, A>(conn: A) -> ServiceResponse<A::Connection>
where
    A: Acquire<'a, Database = Sqlite>,
{
    Ok(conn
        .acquire()
        .await
        .context("Acquire database connection error")?)
}

pub trait ToServiceResponse<T> {
    fn to_service_response(self) -> ServiceResponse<T>;
}

impl<T> ToServiceResponse<T> for Result<T, sqlx::Error> {
    fn to_service_response(self) -> ServiceResponse<T> {
        Ok(self.context("Database error")?)
    }
}
