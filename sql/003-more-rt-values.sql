BEGIN;

ALTER TABLE rt.weight        ADD CONSTRAINT rt_weight_weight_unique          UNIQUE (weight);
ALTER TABLE rt.timescale     ADD CONSTRAINT rt_timescale_time_unique         UNIQUE (time);
ALTER TABLE rt.action_type   ADD CONSTRAINT rt_action_type_ordering_unique   UNIQUE (ordering) DEFERRABLE;
ALTER TABLE rt.action_status ADD CONSTRAINT rt_action_status_ordering_unique UNIQUE (ordering) DEFERRABLE;


UPDATE rt.action_type SET ordering = ordering + 1 WHERE ordering >= 4;
INSERT INTO rt.action_type (description, ordering) VALUES
    ('meta', 4),
    ('purchase', 7);

UPDATE rt.action_status SET ordering = ordering + 1 WHERE ordering >= 4;
INSERT INTO rt.action_status (description, ordering) VALUES
    ('waiting', 4);


UPDATE version SET version = 3;
COMMIT;