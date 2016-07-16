BEGIN;

    DROP TABLE idea;
    DROP TABLE project;

    UPDATE version SET current = 0;

COMMIT;
