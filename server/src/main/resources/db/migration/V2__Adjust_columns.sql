ALTER TABLE concept RENAME id TO uuid;
ALTER TABLE concept RENAME created_at TO create_time;
ALTER TABLE concept RENAME updated_at TO update_time;

ALTER TABLE concept_link RENAME id TO uuid;
UPDATE concept_link
  SET directionalp =
    CASE WHEN directionalp = true
      THEN false
      ELSE true
    END;

ALTER TABLE concept_link RENAME directionalp TO is_mutual;
ALTER TABLE concept_link RENAME created_at TO create_time;
ALTER TABLE concept_link RENAME updated_at TO update_time;
