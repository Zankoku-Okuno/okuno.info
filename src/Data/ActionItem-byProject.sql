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
    project_id = ? AND
    rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active')
ORDER BY
    rt.action_status.description = 'active' DESC,
    CASE
        WHEN (deadline - time) :: date <= current_date THEN TRUE
        ELSE FALSE
    END DESC,
    rt.action_status.description = 'waiting' DESC,
    rt.action_status.description = 'queued' DESC,
    EXTRACT(EPOCH FROM time) / (24*3600) / weight ::float ASC,
    last_accessed_on DESC;