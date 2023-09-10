mod domain;
use std::{any::Any, marker::PhantomData, sync::Arc};

pub use domain::*;
use sea_orm::DatabaseConnection;

mod store;
