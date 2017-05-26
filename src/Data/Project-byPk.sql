SELECT
    project.id,
    name,
    mission,
    rt.action_type.description,
    rt.action_status.description
FROM project
    JOIN rt.action_type ON (action_type_id = action_type.id)
    JOIN rt.weight ON (weight_id = weight.id)
    JOIN rt.timescale ON (timescale_id = timescale.id)
    JOIN rt.action_status ON (action_status_id = action_status.id)
WHERE
    project.id = ?;