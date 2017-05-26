-- FIXME add defaults in the database for the timing stuff
INSERT INTO project (
    name,
    mission,
    action_type_id,
    action_status_id
) (
    SELECT
        ?,
        ?,
        rt.action_type.id,
        rt.action_status.id
    FROM rt.action_type, rt.action_status
    WHERE
        rt.action_type.description = ? AND
        rt.action_status.description = ?
)
RETURNING id;