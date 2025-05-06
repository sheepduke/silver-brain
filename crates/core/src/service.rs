// ============================================================
//  Repo Name
// ============================================================

use derive_more::{From, FromStr, Into};

#[derive(From, Into, FromStr)]
pub struct RepoName(String);

// ============================================================
//  Request Context
// ============================================================

pub struct RequestContext {
    pub repo_name: RepoName,
}
