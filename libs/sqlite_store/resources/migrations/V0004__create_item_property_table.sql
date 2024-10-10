CREATE TABLE IF NOT EXISTS item_property(
  item_id TEXT NOT NULL,
  key TEXT NOT NULL,
  value TEXT NOT NULL,
  create_time TEXT NOT NULL,
  update_time TEXT NOT NULL,

  PRIMARY KEY(item_id, key),

  CONSTRAINT fk_item_id
    FOREIGN KEY (item_id)
    REFERENCES item(id)
    ON DELETE CASCADE
);
