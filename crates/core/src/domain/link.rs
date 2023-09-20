use typed_builder::TypedBuilder;

use crate::EntryId;

// ============================================================
//  Link
// ============================================================

#[derive(Clone, Default, TypedBuilder, Debug)]
pub struct Link {
    pub id: LinkId,
    pub source: EntryId,
    pub target: EntryId,
    pub annotation: String,
}

// ============================================================
//  LinkId
// ============================================================

#[derive(Clone, Default, Debug)]
pub struct LinkId(pub String);
