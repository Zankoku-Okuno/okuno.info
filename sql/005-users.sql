BEGIN;


CREATE TABLE client (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE
);

CREATE TABLE action_item__client (
    client_id INTEGER REFERENCES client(id),
    action_item_id INTEGER REFERENCES action_item(id),
    PRIMARY KEY (client_id, action_item_id)
);
CREATE TABLE project__client (
    client_id INTEGER REFERENCES client(id),
    project_id INTEGER REFERENCES project(id),
    PRIMARY KEY (client_id, project_id)
);


INSERT INTO client (id, name, email) VALUES
    (0, 'okuno', 'okuno54@gmail.com');
INSERT INTO project__client (client_id, project_id)
    (SELECT 0, id FROM project);
INSERT INTO action_item__client (client_id, action_item_id)
    (SELECT 0, id FROM action_item);


UPDATE version SET version = 5;
COMMIT;