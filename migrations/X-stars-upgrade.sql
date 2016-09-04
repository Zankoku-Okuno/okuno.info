ALTER TABLE action
ADD COLUMN starred INT NOT NULL
                       DEFAULT 0
                       CHECK (starred IN (0, 1));

ALTER TABLE project
ADD COLUMN starred INT NOT NULL
                       DEFAULT 0
                       CHECK (starred IN (0, 1));

UPDATE version SET current = 4;
