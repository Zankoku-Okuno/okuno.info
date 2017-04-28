UPDATE version SET version = 1;

CREATE TABLE action_item (
    id INTEGER PRIMARY KEY,
    text TEXT NOT NULL,
    created TIMESTAMPTZ NOT NULL,
    redline TIMESTAMPTZ, -- time to worry about completion
    deadline TIMESTAMPTZ -- time to get it complete
);