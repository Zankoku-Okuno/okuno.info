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
    rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active')
ORDER BY
    CASE
        WHEN (deadline - time) :: date <= current_date THEN TRUE
        ELSE FALSE
    END DESC,
    rt.action_status.description = 'active' DESC,
    rt.action_status.description = 'waiting' DESC,
    rt.action_status.description = 'queued' DESC,
    EXTRACT(EPOCH FROM time) / (24*3600) / weight ::float ASC,
    last_accessed_on DESC;