CREATE TABLE IF NOT EXISTS item(
    id TEXT PRIMARY KEY,
    props JSON NOT NULL
);

CREATE TABLE IF NOT EXISTS item_child(
    parent TEXT NOT NULL,
    child TEXT NOT NULL,
    create_time TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS item_reference(
    id TEXT PRIMARY KEY,
    source TEXT NOT NULL,
    target TEXT NOT NULL,
    annotation TEXT NOT NULL,
    create_time TEXT NOT NULL,
    update_time TEXT NOT NULL
);
