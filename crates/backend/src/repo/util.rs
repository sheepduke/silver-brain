use anyhow::Context;
use silver_brain_core::ServiceResponse;
use sqlx::migrate::MigrateError;

pub trait ToServiceResponse<T> {
    fn to_service_response(self) -> ServiceResponse<T>;
}

impl<T> ToServiceResponse<T> for Result<T, sqlx::Error> {
    fn to_service_response(self) -> ServiceResponse<T> {
        Ok(self.context("Database error")?)
    }
}

impl<T> ToServiceResponse<T> for Result<T, MigrateError> {
    fn to_service_response(self) -> ServiceResponse<T> {
        Ok(self.context("Migration error")?)
    }
}
