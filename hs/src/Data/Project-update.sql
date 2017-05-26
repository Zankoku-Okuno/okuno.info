UPDATE project SET
    name = ?,
    mission = ?,
    action_type_id = (SELECT id from rt.action_type WHERE description = ?),
    action_status_id = (SELECT id from rt.action_status WHERE description = ?),
    last_accessed_on = current_date,
    last_update_on = current_date
WHERE
    project.id = ?
RETURNING id;