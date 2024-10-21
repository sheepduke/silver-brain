CREATE TABLE IF NOT EXISTS item(
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  content_type TEXT NOT NULL,
  content TEXT NOT NULL,
  create_time TEXT NOT NULL,
  update_time TEXT NOT NULL
);
