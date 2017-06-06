{-# LANGUAGE TemplateHaskell #-}
module Data.Project
    ( Project(..)
    , Data.Project.all, byClient
    , byPk
    , create, update
    ) where

import ClassyPrelude

import Data.Db
import Data.Client (Client)
import Util (fileStr)


data Project = Project
    { name :: Text
    , mission :: Text
    , action_type :: Text
    , action_status :: Text
    } deriving (Show)

all :: Sql [Stored Project]
all = (xformRow <$>) <$> query [pgSQL|
    SELECT
        project.id,
        name,
        mission,
        rt.action_type.description,
        rt.action_status.description
    FROM project
        JOIN rt.action_type ON (action_type_id = action_type.id)
        JOIN rt.action_status ON (action_status_id = action_status.id)
    ORDER BY
        rt.action_status.description = 'active' DESC,
        name ASC;
    |]

byPk :: Pk Project -> Sql (Maybe (Stored Project))
byPk pk = xform <$> query [pgSQL|
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
        project.id = ${unPk pk};|]
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

byClient :: Stored Client -> Sql [Stored Project]
byClient client = (xformRow <$>) <$> query [pgSQL|
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
        project.id IN (SELECT project_id FROM project__client WHERE client_id = ${unPk $ thePk client})
    ORDER BY
        rt.action_status.description = 'active' DESC,
        name ASC;|]

create :: Stored Client -> Project -> Sql (Stored Project)
create client project@(Project{..}) = do
    ids <- query [pgSQL|
        -- FIXME add defaults in the database for the timing stuff
        INSERT INTO project (
            name,
            mission,
            action_type_id,
            action_status_id
        ) (
            SELECT
                ${name},
                ${mission},
                rt.action_type.id,
                rt.action_status.id
            FROM rt.action_type, rt.action_status
            WHERE
                rt.action_type.description = ${action_type} AND
                rt.action_status.description = ${action_status}
        )
        RETURNING id;|]
    project <- case ids of
        [pk] -> pure $ Stored (Pk pk) project
        _ -> error "sql insert failed"
    execute [pgSQL|
        INSERT INTO project__client (project_id, client_id)
        VALUES (${unPk $ thePk project}, ${unPk $ thePk client});|]
    pure project

update :: Stored Project -> Sql (Maybe (Stored Project))
update item@(Stored pk Project{..}) = do
    ids <- query [pgSQL|
        UPDATE project SET
            name = ${name},
            mission = ${mission},
            action_type_id = (SELECT id from rt.action_type WHERE description = ${action_type}),
            action_status_id = (SELECT id from rt.action_status WHERE description = ${action_status}),
            last_accessed_on = current_date,
            last_update_on = current_date
        WHERE
            project.id = ${unPk pk}
        RETURNING id;|]
    pure $ case ids of
        [] -> Nothing
        [pk :: Int32] -> Just item
        _ -> error "sql insert failed"

xformRow (id, name, mission, action_type, action_status) = Stored (Pk id) $ Project{..}