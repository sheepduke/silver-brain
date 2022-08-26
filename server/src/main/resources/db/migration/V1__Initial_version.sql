create table if not exists concept(
  id integer primary key autoincrement,
  uuid varchar(64) not null,
  name varchar(1024) not null,
  content varchar(1024) not null,
  content_format varchar(16) not null,
  created_at timestamp,
  updated_at timestamp
);

create table if not exists concept_relation (
  id integer primary key autoincrement,
  source varchar(64) not null,
  target varchar(64) not null,
  created_at timestamp,
  updated_at timestamp
);
