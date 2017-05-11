BEGIN;

CREATE TABLE version (
    current INT NOT NULL
);

INSERT INTO version (current) VALUES (0);
COMMIT;
