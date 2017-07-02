{-# LANGUAGE TemplateHaskell #-}
module Data.ActionItem
    ( ActionItem(..)
    , Data.ActionItem.all, dashboard
    , byPk
    , create, update
    ) where

import ClassyPrelude
import Util (fileStr)
import Data.Db

import Data.Project (Project)
import Data.Client (Client)


data ActionItem = ActionItem
    { text :: Text
    , action_status :: Text
    , weight :: Text
    , timescale :: Text
    , deadline :: Maybe Day
    } deriving (Show)


all :: Sql [Stored ActionItem]
all = (xformRow <$>) <$> query [pgSQL|
    SELECT
        action_item.id,
        text,
        rt.action_status.description,
        rt.weight.description,
        rt.timescale.description,
        deadline
    FROM action_item
        JOIN rt.weight ON (weight_id = weight.id)
        JOIN rt.timescale ON (timescale_id = timescale.id)
        JOIN rt.action_status ON (action_status_id = action_status.id)
    WHERE
        rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active');|]

dashboard :: Stored Client -> Maybe (Stored Project) -> Sql [Stored ActionItem]
dashboard client project = (xformRow <$>) <$> rows
    where
    rows = case project of
        Nothing -> query [pgSQL|
            WITH awareness AS (
                SELECT *
                FROM awareness
                WHERE client_id = ${unPk $ thePk client}
            )
            SELECT
                action_item.id,
                text,
                rt.action_status.description,
                rt.weight.description,
                rt.timescale.description,
                deadline
            FROM
                action_item
                    JOIN rt.weight ON (weight_id = weight.id)
                    JOIN rt.timescale ON (timescale_id = timescale.id)
                    JOIN rt.action_status ON (action_status_id = action_status.id),
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
                    FROM awareness
                    WHERE awareness.project_id IS NULL
                ) AND
                rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active')
            ORDER BY
                rt.action_status.description = 'active' DESC,
                rt.action_status.description = 'waiting' DESC,
                rt.action_status.description = 'queued' DESC,
                value_for_time + crunch + random() DESC,
                last_accessed_on DESC;|]
        Just project -> query [pgSQL|
            WITH awareness AS (
                SELECT *
                FROM awareness
                WHERE client_id = ${unPk $ thePk client}
            )
            SELECT
                action_item.id,
                text,
                rt.action_status.description,
                rt.weight.description,
                rt.timescale.description,
                deadline
            FROM
                action_item
                    JOIN rt.weight ON (weight_id = weight.id)
                    JOIN rt.timescale ON (timescale_id = timescale.id)
                    JOIN rt.action_status ON (action_status_id = action_status.id),
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
                    FROM awareness
                    WHERE awareness.project_id = ${unPk $ thePk project}
                ) AND
                rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active')
            ORDER BY
                rt.action_status.description = 'active' DESC,
                rt.action_status.description = 'waiting' DESC,
                rt.action_status.description = 'queued' DESC,
                value_for_time + crunch + random() DESC,
                last_accessed_on DESC;|]

byPk :: Pk ActionItem -> Sql (Maybe (Stored ActionItem))
byPk pk = xform <$> query [pgSQL|
    SELECT
        action_item.id,
        text,
        rt.action_status.description,
        rt.weight.description,
        rt.timescale.description,
        deadline
    FROM action_item
        JOIN rt.weight ON (weight_id = weight.id)
        JOIN rt.timescale ON (timescale_id = timescale.id)
        JOIN rt.action_status ON (action_status_id = action_status.id)
    WHERE
        action_item.id = ${unPk pk};|]
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

create :: Stored Client -> ActionItem -> Maybe (Stored Project) -> Sql (Stored ActionItem)
create client item@(ActionItem{..}) project = do
    ids <- query [pgSQL|
        -- FIXME add defaults in the database for the timing stuff
        INSERT INTO action_item (
            text,
            action_status_id,
            weight_id,
            timescale_id,
            deadline
        ) (
            SELECT
                ${text},
                rt.action_status.id,
                rt.weight.id,
                rt.timescale.id,
                ${deadline}
            FROM rt.weight, rt.timescale, rt.action_status
            WHERE
                rt.action_status.description = ${action_status} AND
                rt.weight.description = ${weight} AND
                rt.timescale.description = ${timescale}
        )
        RETURNING id;|]
    item <- case ids of
        [pk] -> pure $ Stored (Pk pk) item
        _ -> error "sql insert failed"
    -- TODO add to a project
    execute [pgSQL|
        INSERT INTO awareness (
            action_item_id,
            client_id,
            project_id
        ) VALUES (
            ${unPk $ thePk item},
            ${unPk $ thePk client},
            ${unPk . thePk <$> project}
        );|]
    pure item

update :: Stored ActionItem -> Sql (Maybe (Stored ActionItem))
update item@(Stored pk ActionItem{..}) = do
    ids <- query [pgSQL|
        UPDATE action_item SET
            text = ${text},
            action_status_id = (SELECT id from rt.action_status WHERE description = ${action_status}),
            weight_id = (SELECT id from rt.weight WHERE description = ${weight}),
            timescale_id = (SELECT id from rt.timescale WHERE description = ${timescale}),
            deadline = ${deadline},
            last_accessed_on = current_date,
            last_update_on = current_date
        WHERE
            action_item.id = ${unPk pk}
        RETURNING id;|]
    -- TODO update project
    pure $ case ids of
        [] -> Nothing
        [pk :: Int32] -> Just item
        _ -> error "sql insert failed"


xformRow (id, text, action_status, weight, timescale, deadline) =
    Stored (Pk id) ActionItem{..}