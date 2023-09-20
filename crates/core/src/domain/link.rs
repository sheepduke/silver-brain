use time::OffsetDateTime;
use typed_builder::TypedBuilder;

use crate::EntryId;

// ============================================================
//  Link
// ============================================================

#[derive(Clone, TypedBuilder, Debug)]
pub struct Link {
    pub id: LinkId,
    pub source: EntryId,
    pub target: EntryId,
    pub annotation: String,
    pub create_time: OffsetDateTime,
    pub update_time: OffsetDateTime,
}

impl Default for Link {
    fn default() -> Self {
        Self {
            id: Default::default(),
            source: Default::default(),
            target: Default::default(),
            annotation: Default::default(),
            create_time: OffsetDateTime::now_utc(),
            update_time: OffsetDateTime::now_utc(),
        }
    }
}

// ============================================================
//  LinkId
// ============================================================

#[derive(Clone, Default, Debug)]
pub struct LinkId(pub String);
