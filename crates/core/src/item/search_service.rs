use crate::*;

use super::ItemLoadOptions;

pub trait SearchService {
    async fn search_items(
        &self,
        context: &RequestContext,
        search: &str,
        options: &ItemLoadOptions,
    ) -> ServiceResponse<Vec<Item>>;
}
