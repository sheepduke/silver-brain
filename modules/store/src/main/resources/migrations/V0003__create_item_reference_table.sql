CREATE TABLE IF NOT EXISTS item_reference(
  id TEXT PRIMARY KEY NOT NULL,
  source TEXT NOT NULL,
  target TEXT NOT NULL,
  annotation TEXT NOT NULL,
  create_time TEXT NOT NULL,
  update_time TEXT NOT NULL,

  CONSTRAINT fk_source
    FOREIGN KEY (source)
    REFERENCES item(id)
    ON DELETE CASCADE,

  CONSTRAINT fk_target
    FOREIGN KEY (target)
    REFERENCES item(id)
    ON DELETE CASCADE
);
