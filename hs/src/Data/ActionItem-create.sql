-- FIXME add defaults in the database for the timing stuff
INSERT INTO action_item (
    text,
    action_type_id,
    weight_id,
    timescale_id,
    deadline,
    action_status_id
) (
    SELECT
        ?,
        rt.action_type.id,
        rt.weight.id,
        rt.timescale.id,
        ?,
        rt.action_status.id
    FROM rt.action_type, rt.weight, rt.timescale, rt.action_status
    WHERE
        rt.action_type.description = ? AND
        rt.weight.description = ? AND
        rt.timescale.description = ? AND
        rt.action_status.description = ?
)
RETURNING id;