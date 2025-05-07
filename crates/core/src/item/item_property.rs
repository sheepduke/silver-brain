#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemProperty {
    pub key: String,
    pub value: String,
}

impl ItemProperty {
    pub fn new(key: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            value: value.into(),
        }
    }
}
