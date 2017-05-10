BEGIN;

CREATE SCHEMA rt
    CREATE TABLE rt.timescale (
        id SERIAL PRIMARY KEY,
        time INTERVAL NOT NULL,
        description TEXT NOT NULL
    )
    CREATE TABLE rt.weight (
        id SERIAL PRIMARY KEY,
        weight INTEGER NOT NULL,
        description TEXT NOT NULL
    )
    CREATE TABLE rt.action_type (
        id SERIAL PRIMARY KEY,
        description TEXT NOT NULL,
        ordering INTEGER NOT NULL
    )
    CREATE TABLE rt.action_status (
        id SERIAL PRIMARY KEY,
        description TEXT NOT NULL,
        ordering INTEGER NOT NULL
    );

CREATE TABLE action_item (
    id SERIAL PRIMARY KEY,
    text TEXT NOT NULL,
    action_type_id INTEGER NOT NULL REFERENCES rt.action_type,
    weight_id INTEGER NOT NULL REFERENCES rt.weight,
    timescale_id INTEGER NOT NULL REFERENCES rt.timescale,
    deadline DATE,
    action_status_id INTEGER NOT NULL REFERENCES rt.action_status,
    -- timing data for caching
    created_on DATE NOT NULL,
    last_accessed_on DATE NOT NULL,
    last_update_on DATE NOT NULL
);


INSERT INTO rt.timescale (description, time) VALUES
    ('hours', interval 'P1D'),
    ('days', interval 'P7D'),
    ('weeks', interval 'P4W'),
    ('months', interval 'P3M'),
    ('years', interval 'P2Y');

INSERT INTO rt.weight (description, weight) VALUES
    ('trivial', 1),
    ('minor', 3),
    ('medium', 10),
    ('major', 30);

INSERT INTO rt.action_type (description, ordering) VALUES
    ('negentropy', 1),
    ('intel', 2),
    ('decision', 3),
    ('artifact', 4),
    ('learning', 5);

INSERT INTO rt.action_status (description, ordering) VALUES
    ('proposed', 1),
    ('queued', 2),
    ('active', 3),
    ('complete', 4),
    ('dismissed', 5);

UPDATE version SET version = 1;
COMMIT;