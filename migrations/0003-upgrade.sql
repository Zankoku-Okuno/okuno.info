CREATE TABLE IF NOT EXISTS epigram (
    id INTEGER PRIMARY KEY,
    text TEXT NOT NULL,
    credit TEXT
);

UPDATE version SET current = 3;
