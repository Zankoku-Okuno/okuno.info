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


data Project = Project
    { name :: Text
    , mission :: Text
    , action_status :: Text
    } deriving (Show)

all :: Sql [Stored Project]
all = (xformRow <$>) <$> query [pgSQL|
    SELECT
        project.id,
        name,
        mission,
        rt.action_status.description
    FROM project
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
        rt.action_status.description
    FROM project
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
        rt.action_status.description
    FROM project
        JOIN rt.action_status ON (action_status_id = action_status.id)
    WHERE
        owner = ${unPk $ thePk client}
    ORDER BY
        rt.action_status.description = 'active' DESC,
        name ASC;|]

create :: Stored Client -> Project -> Sql (Stored Project)
create client project@(Project{..}) = do
    ids <- query [pgSQL|
        -- FIXME add defaults in the database for the timing stuff
        INSERT INTO project (
            name,
            owner,
            mission,
            action_status_id
        ) (
            SELECT
                ${name},
                ${unPk $ thePk client},
                ${mission},
                rt.action_status.id
            FROM rt.action_status
            WHERE
                rt.action_status.description = ${action_status}
        )
        RETURNING id;|]
    project <- case ids of
        [pk] -> pure $ Stored (Pk pk) project
        _ -> error "sql insert failed"
    pure project

update :: Stored Project -> Sql (Maybe (Stored Project))
update item@(Stored pk Project{..}) = do
    ids <- query [pgSQL|
        UPDATE project SET
            name = ${name},
            mission = ${mission},
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

xformRow (id, name, mission, action_status) = Stored (Pk id) $ Project{..}