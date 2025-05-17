use crate::*;

use super::ItemLoadOptions;

pub trait ItemSearchService {
    fn search_items(
        &self,
        context: &RequestContext,
        search: &str,
        options: &ItemLoadOptions,
    ) -> impl Future<Output = ServiceResponse<Vec<Item>>> + Send;
}
