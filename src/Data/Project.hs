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
    , lifecycle :: Text
    } deriving (Show)

all :: Sql [Stored Project]
all = (xformRow <$>) <$> query [pgSQL|
    SELECT
        project.id,
        name,
        mission,
        rt.lifecycle.description
    FROM project
        JOIN rt.lifecycle ON (lifecycle_id = lifecycle.id)
    ORDER BY
        rt.lifecycle.description = 'active' DESC,
        name ASC;
    |]

byPk :: Pk Project -> Sql (Maybe (Stored Project))
byPk pk = xform <$> query [pgSQL|
    SELECT
        project.id,
        name,
        mission,
        rt.lifecycle.description
    FROM project
        JOIN rt.lifecycle ON (lifecycle_id = lifecycle.id)
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
        rt.lifecycle.description
    FROM project
        JOIN rt.lifecycle ON (lifecycle_id = lifecycle.id)
    WHERE
        owner = ${unPk $ thePk client}
    ORDER BY
        rt.lifecycle.description = 'active' DESC,
        name ASC;|]

create :: Stored Client -> Project -> Sql (Stored Project)
create client project@(Project{..}) = do
    ids <- query [pgSQL|
        -- FIXME add defaults in the database for the timing stuff
        INSERT INTO project (
            name,
            owner,
            mission,
            lifecycle_id
        ) (
            SELECT
                ${name},
                ${unPk $ thePk client},
                ${mission},
                rt.lifecycle.id
            FROM rt.lifecycle
            WHERE
                rt.lifecycle.description = ${lifecycle}
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
            lifecycle_id = (SELECT id from rt.lifecycle WHERE description = ${lifecycle}),
            last_accessed_on = current_date,
            last_update_on = current_date
        WHERE
            project.id = ${unPk pk}
        RETURNING id;|]
    pure $ case ids of
        [] -> Nothing
        [pk :: Int32] -> Just item
        _ -> error "sql insert failed"

xformRow (id, name, mission, lifecycle) = Stored (Pk id) $ Project{..}