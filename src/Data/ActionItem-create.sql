-- FIXME add defaults in the database for the timing stuff
INSERT INTO action_item (
    text,
    project_id,
    action_type_id,
    action_status_id,
    weight_id,
    timescale_id,
    deadline,
    behalf_of
) (
    SELECT
        ?,
        ?,
        rt.action_type.id,
        rt.action_status.id,
        rt.weight.id,
        rt.timescale.id,
        ?,
        ?
    FROM rt.action_type, rt.weight, rt.timescale, rt.action_status
    WHERE
        rt.action_type.description = ? AND
        rt.action_status.description = ? AND
        rt.weight.description = ? AND
        rt.timescale.description = ?
)
RETURNING id;