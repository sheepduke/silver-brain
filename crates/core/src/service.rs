// ============================================================
//  Repo Name
// ============================================================

use derive_more::{Deref, From, FromStr, Into};
use typed_builder::TypedBuilder;

#[derive(Debug, From, Into, FromStr, Deref)]
pub struct RepoName(String);

// ============================================================
//  Request Context
// ============================================================

#[derive(Debug, TypedBuilder)]
pub struct RequestContext {
    #[builder(setter(into))]
    pub repo_name: RepoName,
}
