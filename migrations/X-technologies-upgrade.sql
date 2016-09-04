CREATE TABLE technology (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    learn_project_id INTEGER NOT NULL REFERENCES project(id)
);

CREATE TABLE tech_notes (
    id INTEGER PRIMARY KEY,
    technology_id INTEGER NOT NULL REFERENCES technology(id),
    text TEXT NOT NULL,
    quick_ix INTEGER CHECK (quick_ix >= 0)
);

UPDATE version SET current = 4;
