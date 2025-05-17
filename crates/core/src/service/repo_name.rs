use std::str::FromStr;

use derive_more::{Deref, From, Into};

use crate::ServiceError;

#[derive(Debug, From, Into, Deref)]
pub struct RepoName(String);

impl FromStr for RepoName {
    type Err = ServiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.to_string()))
    }
}
