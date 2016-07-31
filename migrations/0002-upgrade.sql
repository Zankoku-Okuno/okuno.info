CREATE TABLE IF NOT EXISTS action (
    id INTEGER PRIMARY KEY,
    project_id INTEGER NOT NULL REFERENCES project(id),
    text TEXT NOT NULL,
    created TEXT NOT NULL, -- ISO8601 datetime (YYYY-MM-DDThh:mm:ss+00:00)
    completed TEXT -- ISO8601 datetime (YYYY-MM-DDThh:mm:ss+00:00)
);

CREATE TABLE IF NOT EXISTS hat (
    id INTEGER PRiMARY KEY,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS project_hat_assoc (
    project_id INTEGER NOT NULL REFERENCES project(id),
    hat_id INTEGER NOT NULL REFERENCES hat(id),
    PRIMARY KEY (project_id, hat_id)
);

UPDATE version SET current = 2;
