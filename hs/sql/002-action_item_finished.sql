UPDATE version SET version = 2;

ALTER TABLE action_item
ADD COLUMN finish_time TIMESTAMPTZ,
ADD COLUMN completed BOOLEAN NOT NULL;