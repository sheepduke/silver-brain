create table if not exists concept_relation(
  source text,
  target text,
  created_at not null,
  updated_at not null
);

create table if not exists concept(
  uuid text primary key,
  name text not null,
  content_type text not null,
  content text not null,
  created_at not null,
  updated_at not null
);
