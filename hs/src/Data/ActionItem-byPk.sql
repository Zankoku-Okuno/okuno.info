SELECT
    action_item.id,
    text,
    rt.action_type.description,
    rt.weight.description,
    rt.timescale.description,
    deadline,
    rt.action_status.description
FROM action_item
    JOIN rt.action_type ON (action_type_id = action_type.id)
    JOIN rt.weight ON (weight_id = weight.id)
    JOIN rt.timescale ON (timescale_id = timescale.id)
    JOIN rt.action_status ON (action_status_id = action_status.id)
WHERE
    action_item.id = ?;