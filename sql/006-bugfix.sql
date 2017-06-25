BEGIN;

UPDATE rt.weight SET weight = 2 WHERE description = 'minor';
UPDATE rt.weight SET weight = 5 WHERE description = 'medium';
UPDATE rt.weight SET weight = 11 WHERE description = 'major';

ALTER TABLE rt.timescale ADD CONSTRAINT rt_timescale_time_gte_1 CHECK (EXTRACT(EPOCH FROM time)/3600 >= 1);

UPDATE version SET version = 6;
COMMIT;