use typed_builder::TypedBuilder;

use super::RepoName;

#[derive(Debug, TypedBuilder)]
pub struct RequestContext {
    #[builder(setter(into))]
    pub repo_name: RepoName,
}
