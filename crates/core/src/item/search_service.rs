use crate::*;

use super::ItemLoadOptions;

pub trait SearchService {
    async fn search(
        &self,
        context: &RequestContext,
        search: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Vec<Item>>;
}
