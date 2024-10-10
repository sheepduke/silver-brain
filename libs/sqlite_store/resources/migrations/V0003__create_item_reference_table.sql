CREATE TABLE IF NOT EXISTS item_link(
  parent TEXT NOT NULL,
  child TEXT NOT NULL,
  create_time TEXT NOT NULL,
  update_time TEXT NOT NULL,

  PRIMARY KEY(parent, child),

  CONSTRAINT fk_parent
    FOREIGN KEY (parent)
    REFERENCES item(id)
    ON DELETE CASCADE,

  CONSTRAINT fk_child
    FOREIGN KEY (child)
    REFERENCES item(id)
    ON DELETE CASCADE
);
