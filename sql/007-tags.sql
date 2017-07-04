BEGIN;



CREATE TABLE tag (
    id       SERIAL           PRIMARY KEY,
    owner    INTEGER NOT NULL REFERENCES client,
    category TEXT,
    name     TEXT    NOT NULL,

    CONSTRAINT tag_unique_cat_name UNIQUE (owner, category, name)
);
CREATE UNIQUE INDEX tag_unique_name ON tag (owner, name)
       WHERE category IS NULL;

CREATE TABLE awareness (
    id             SERIAL           PRIMARY KEY,
    action_item_id INTEGER NOT NULL REFERENCES action_item,
    client_id      INTEGER NOT NULL REFERENCES client,

    project_id     INTEGER          REFERENCES project,

    -- TODO permissions, given vs. accepted
    CONSTRAINT awareness_assoc_unique UNIQUE (action_item_id, client_id)
);
CREATE TABLE awareness__tag (
    awareness_id INTEGER NOT NULL REFERENCES awareness,
    tag_id       INTEGER NOT NULL REFERENCES tag,

    PRIMARY KEY (awareness_id, tag_id)
);


-- populate awareness table
INSERT INTO awareness (action_item_id, client_id)
SELECT action_item_id, client_id
FROM action_item__client;

DROP TABLE action_item__client;

UPDATE awareness
SET project_id = action_item.project_id
FROM action_item
WHERE awareness.action_item_id = action_item.id;

ALTER TABLE action_item
    DROP COLUMN project_id;


-- move project ownership location
ALTER TABLE project
    ADD COLUMN owner INTEGER REFERENCES client;

UPDATE project
SET owner = project__client.client_id
FROM project__client
WHERE project.id = project__client.project_id;

ALTER TABLE project
    ALTER COLUMN owner SET NOT NULL,
    DROP CONSTRAINT project_name_key,
    ADD CONSTRAINT project_unique_name UNIQUE (name, owner);
DROP TABLE project__client;


-- move action types into tags
INSERT INTO tag (owner, name)
(
    SELECT client.id, description
    FROM rt.action_type
        JOIN client ON TRUE
);

INSERT INTO awareness__tag (awareness_id, tag_id)
(
    SELECT awareness.id, tag.id
    FROM action_item
        JOIN awareness ON (awareness.action_item_id = action_item.id)
        JOIN rt.action_type ON (action_item.action_type_id = rt.action_type.id)
        JOIN tag ON (rt.action_type.description = tag.name)
);

ALTER TABLE action_item
    DROP COLUMN action_type_id;

ALTER TABLE project
    DROP COLUMN action_type_id;

DROP TABLE rt.action_type;


-- move behalf_of into tags
INSERT INTO tag (owner, name)
(
    SELECT DISTINCT client.id, 'for ' || behalf_of
    FROM action_item
        JOIN client ON TRUE
    WHERE behalf_of IS NOT NULL
);

ALTER TABLE action_item
    DROP COLUMN behalf_of;


-- clean up any unused tags
DELETE FROM tag
WHERE tag.id NOT IN (
    SELECT tag_id
    FROM awareness__tag
);



UPDATE version SET version = 7;
COMMIT;
