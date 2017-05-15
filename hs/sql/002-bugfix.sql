BEGIN;


ALTER TABLE action_item
    ALTER COLUMN created_on       SET DEFAULT current_date,
    ALTER COLUMN last_accessed_on SET DEFAULT current_date,
    ALTER COLUMN last_update_on   SET DEFAULT current_date;

ALTER TABLE rt.timescale     ADD CONSTRAINT rt_timescale_description_unique     UNIQUE (description);
ALTER TABLE rt.weight        ADD CONSTRAINT rt_weight_description_unique        UNIQUE (description);
ALTER TABLE rt.action_type   ADD CONSTRAINT rt_action_type_description_unique   UNIQUE (description);
ALTER TABLE rt.action_status ADD CONSTRAINT rt_action_status_description_unique UNIQUE (description);


UPDATE version SET version = 1;
COMMIT;