BEGIN;



-- accidental over-application of tags
DELETE
FROM awareness__tag USING aware_action_item, tag
WHERE
    tag.id = tag_id AND
    awareness_id = aware_action_item.id AND
    owner != client_id;

UPDATE tag
SET name = category || ' ' || name
WHERE category IS NOT NULL;

ALTER TABLE tag
    DROP CONSTRAINT tag_unique_cat_name,
    DROP COLUMN category,
    ADD CONSTRAINT tag_unique_cat_name UNIQUE (owner, name);

ALTER TABLE awareness
    RENAME TO aware_action_item;

ALTER TABLE rt.action_status
    RENAME TO lifecycle;
ALTER TABLE action_item
    RENAME COLUMN action_status_id TO lifecycle_id;
ALTER TABLE project
    RENAME COLUMN action_status_id TO lifecycle_id;



UPDATE version SET version = 8;
COMMIT;