CREATE TABLE project (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT NOT NULL
);

CREATE TABLE idea (
    id INTEGER PRIMARY KEY,
    text TEXT NOT NULL,
    project_id INTEGER REFERENCES project(id),
    crankfile INTEGER NOT NULL DEFAULT 0,

    created TEXT NOT NULL, -- ISO8601 datetime (YYYY-MM-DDThh:mm:ss+00:00)
    sorted TEXT -- ISO8601 datetime (YYYY-MM-DDThh:mm:ss+00:00)
);

UPDATE version SET current = 1;

