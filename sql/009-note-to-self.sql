BEGIN;




CREATE TYPE note_state AS ENUM ('Inbox', 'Alive', 'Retired');

CREATE TABLE note (
    id              SERIAL              PRIMARY KEY,
    text            TEXT NOT NULL,
    owner_id        INTEGER NOT NULL    REFERENCES client,
    review_cycles   INTEGER NOT NULL    DEFAULT 0,
    review_status   note_state NOT NULL DEFAULT 'Inbox',
    created_on      DATE NOT NULL       DEFAULT current_date
);





UPDATE version SET version = 9;
COMMIT;