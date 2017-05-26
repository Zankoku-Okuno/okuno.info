BEGIN;


INSERT INTO rt.timescale (description, time) VALUES ('minutes', interval 'PT1H');


CREATE TABLE project (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    mission TEXT NOT NULL,
    action_type_id INTEGER NOT NULL REFERENCES rt.action_type,
    action_status_id INTEGER NOT NULL REFERENCES rt.action_status,
    -- timing data for caching
    created_on DATE NOT NULL DEFAULT current_date,
    last_accessed_on DATE NOT NULL DEFAULT current_date,
    last_update_on DATE NOT NULL DEFAULT current_date
);

ALTER TABLE action_item
    ADD COLUMN project_id INTEGER REFERENCES project,
    ADD COLUMN behalf_of TEXT;


UPDATE version SET version = 4;
COMMIT;