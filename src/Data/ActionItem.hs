{-# LANGUAGE TemplateHaskell #-}
module Data.ActionItem
    ( ActionItem(..)
    , dashboard
    , byPk
    , create, update
    ) where

import ClassyPrelude
import Util (fileStr)
import Data.Db

import Data.Project (Project)
import Data.Client (Client, Username)


data ActionItem = ActionItem
    { text :: Text
    , lifecycle :: Text
    , weight :: Text
    , timescale :: Text
    , deadline :: Maybe Day
    , project_id :: Maybe (Pk Project)
    } deriving (Show)


dashboard :: Stored Client -> Maybe (Stored Project) -> Sql [Stored ActionItem]
dashboard client project = (xformRow <$>) <$> rows
    where
    rows = case project of
        Nothing -> query [pgSQL|
            WITH aware_action_item AS (
                SELECT *
                FROM aware_action_item
                WHERE client_id = ${unPk $ thePk client}
            )
            SELECT
                action_item.id,
                project_id,
                text,
                rt.lifecycle.description,
                rt.weight.description,
                rt.timescale.description,
                deadline
            FROM aware_action_item
                    JOIN action_item ON (action_item_id = action_item.id)
                    JOIN rt.weight ON (weight_id = weight.id)
                    JOIN rt.timescale ON (timescale_id = timescale.id)
                    JOIN rt.lifecycle ON (lifecycle_id = lifecycle.id),
                LATERAL (SELECT
                    weight / (ln(EXTRACT(EPOCH FROM time) / 3600) + 1),
                    CASE
                        WHEN deadline IS NULL THEN 0
                        ELSE 11*(atan((-( (deadline - current_date) / (EXTRACT(EPOCH FROM rt.timescale.time)/(24*3600)) )/1.618)+pi())/pi()+0.5)
                    END
                    ) AS t1(value_for_time, crunch)
            WHERE
                action_item.id IN (
                    SELECT action_item_id
                    FROM aware_action_item
                    WHERE aware_action_item.project_id IS NULL
                ) AND
                rt.lifecycle.description IN ('proposed', 'queued', 'waiting', 'active')
            ORDER BY
                rt.lifecycle.description = 'active' DESC,
                rt.lifecycle.description = 'waiting' DESC,
                rt.lifecycle.description = 'queued' DESC,
                value_for_time + crunch + random() DESC,
                last_accessed_on DESC;|]
        Just project -> query [pgSQL|
            WITH aware_action_item AS (
                SELECT *
                FROM aware_action_item
                WHERE client_id = ${unPk $ thePk client}
            )
            SELECT
                action_item.id,
                project_id,
                text,
                rt.lifecycle.description,
                rt.weight.description,
                rt.timescale.description,
                deadline
            FROM aware_action_item
                    JOIN action_item ON (action_item_id = action_item.id)
                    JOIN rt.weight ON (weight_id = weight.id)
                    JOIN rt.timescale ON (timescale_id = timescale.id)
                    JOIN rt.lifecycle ON (lifecycle_id = lifecycle.id),
                LATERAL (SELECT
                    weight / (ln(EXTRACT(EPOCH FROM time) / 3600) + 1),
                    CASE
                        WHEN deadline IS NULL THEN 0
                        ELSE 11*(atan((-( (deadline - current_date) / (EXTRACT(EPOCH FROM rt.timescale.time)/(24*3600)) )/1.618)+pi())/pi()+0.5)
                    END
                    ) AS t1(value_for_time, crunch)
            WHERE
                action_item.id IN (
                    SELECT action_item_id
                    FROM aware_action_item
                    WHERE aware_action_item.project_id = ${unPk $ thePk project}
                ) AND
                rt.lifecycle.description IN ('proposed', 'queued', 'waiting', 'active')
            ORDER BY
                rt.lifecycle.description = 'active' DESC,
                rt.lifecycle.description = 'waiting' DESC,
                rt.lifecycle.description = 'queued' DESC,
                value_for_time + crunch + random() DESC,
                last_accessed_on DESC;|]

byPk :: (Stored Client, Pk ActionItem) -> Sql (Maybe (Stored ActionItem))
byPk (client, action_item_id) = xform <$> query [pgSQL|
    SELECT
        action_item.id,
        project_id,
        action_item.text,
        rt.lifecycle.description,
        rt.weight.description,
        rt.timescale.description,
        deadline
    FROM aware_action_item
        JOIN action_item ON (action_item_id = action_item.id)
        JOIN rt.weight ON (weight_id = weight.id)
        JOIN rt.timescale ON (timescale_id = timescale.id)
        JOIN rt.lifecycle ON (lifecycle_id = lifecycle.id)
    WHERE
        action_item_id = ${unPk action_item_id} AND
        client_id = ${unPk $ thePk client};|]
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

create :: (Stored Client, ActionItem) -> Sql (Stored ActionItem)
create (client, item@ActionItem{..}) = do
    ids <- query [pgSQL|
        -- FIXME add defaults in the database for the timing stuff
        INSERT INTO action_item (
            text,
            lifecycle_id,
            weight_id,
            timescale_id,
            deadline
        ) (
            SELECT
                ${text},
                rt.lifecycle.id,
                rt.weight.id,
                rt.timescale.id,
                ${deadline}
            FROM rt.weight, rt.timescale, rt.lifecycle
            WHERE
                rt.lifecycle.description = ${lifecycle} AND
                rt.weight.description = ${weight} AND
                rt.timescale.description = ${timescale}
        )
        RETURNING id;|]
    item <- case ids of
        [pk] -> pure $ Stored (Pk pk) item
        _ -> error "sql insert failed"
    -- TODO add to a project
    execute [pgSQL|
        INSERT INTO aware_action_item (
            action_item_id,
            client_id,
            project_id
        ) VALUES (
            ${unPk $ thePk item},
            ${unPk $ thePk client},
            ${unPk <$> project_id}
        );|]
    pure item

update :: (Stored Client, Pk ActionItem) -> ActionItem -> Sql (Maybe (Stored ActionItem))
update (client, pk) item@ActionItem{..} = do
    action_item_ids <- query [pgSQL|
        UPDATE action_item SET
            text = ${text},
            lifecycle_id = (SELECT id from rt.lifecycle WHERE description = ${lifecycle}),
            weight_id = (SELECT id from rt.weight WHERE description = ${weight}),
            timescale_id = (SELECT id from rt.timescale WHERE description = ${timescale}),
            deadline = ${deadline},
            last_accessed_on = current_date,
            last_update_on = current_date
        WHERE
            action_item.id = ${unPk pk}
        RETURNING id;|]
    awareness_ids <- query [pgSQL|
        UPDATE aware_action_item SET
            project_id = ${unPk <$> project_id}
        WHERE
            action_item_id = ${unPk pk} AND
            client_id = ${unPk $ thePk client}
        RETURNING id;|]
    pure $ case zip action_item_ids awareness_ids of
        [] -> Nothing
        [(Pk -> pk :: Pk ActionItem, _ :: Int32)] -> Just $ Stored pk item
        _ -> error "sql update failed"


xformRow (id, (Pk <$>) -> project_id, text, lifecycle, weight, timescale, deadline) =
    Stored (Pk id) ActionItem{..}