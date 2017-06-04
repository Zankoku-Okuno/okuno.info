SELECT
    project.id,
    name,
    mission,
    rt.action_type.description,
    rt.action_status.description
FROM project
    JOIN rt.action_type ON (action_type_id = action_type.id)
    JOIN rt.action_status ON (action_status_id = action_status.id)
WHERE
    project.id IN (SELECT project_id FROM project__client WHERE client_id = ?)
ORDER BY
    rt.action_status.description = 'active' DESC,
    name ASC;