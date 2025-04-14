use std::str::FromStr;

use svix_ksuid::Ksuid;

pub(crate) fn extract_ksuid(s: &str) -> Option<Ksuid> {
    let (_, second) = s.split_at(2);

    Ksuid::from_str(second).ok()
}
