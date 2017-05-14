UPDATE action_item SET
    text = ?,
    action_type_id = (SELECT id from rt.action_type WHERE description = ?),
    weight_id = (SELECT id from rt.weight WHERE description = ?),
    timescale_id = (SELECT id from rt.timescale WHERE description = ?),
    action_status_id = (SELECT id from rt.action_status WHERE description = ?),
    deadline = ?,
    last_accessed_on = current_date,
    last_update_on = current_date
WHERE
    action_item.id = ?
RETURNING id;