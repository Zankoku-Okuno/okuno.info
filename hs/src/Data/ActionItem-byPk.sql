SELECT
    action_item.id,
    text,
    project_id,
    rt.action_type.description,
    rt.action_status.description,
    rt.weight.description,
    rt.timescale.description,
    deadline,
    behalf_of
FROM action_item
    JOIN rt.action_type ON (action_type_id = action_type.id)
    JOIN rt.weight ON (weight_id = weight.id)
    JOIN rt.timescale ON (timescale_id = timescale.id)
    JOIN rt.action_status ON (action_status_id = action_status.id)
WHERE
    action_item.id = ?;