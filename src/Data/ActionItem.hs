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
    , project :: Maybe (Pk Project)
    , action_type :: Text
    , action_status :: Text
    , weight :: Text
    , timescale :: Text
    , deadline :: Maybe Day
    , behalf_of :: Maybe Text
    } deriving (Show)


all :: Sql [Stored ActionItem]
all = (xformRow <$>) <$> query [pgSQL|
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
        rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active');|]

dashboard :: Stored Client -> Maybe (Stored Project) -> Sql [Stored ActionItem]
dashboard (thePk -> client_id) project = (xformRow <$>) <$> rows
    where
    rows = case project of
        Nothing -> query [pgSQL|
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
            FROM
                action_item
                    JOIN rt.action_type ON (action_type_id = action_type.id)
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
                action_item.id IN (SELECT action_item_id FROM action_item__client WHERE client_id = ${unPk client_id}) AND
                project_id IS NULL AND
                rt.action_status.description IN ('proposed', 'queued', 'waiting', 'active')
            ORDER BY
                rt.action_status.description = 'active' DESC,
                rt.action_status.description = 'waiting' DESC,
                rt.action_status.description = 'queued' DESC,
                value_for_time + crunch + random() DESC,
                last_accessed_on DESC;|]
        Just (thePk -> project_id) -> query [pgSQL|
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
            FROM
                action_item
                    JOIN rt.action_type ON (action_type_id = action_type.id)
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
                action_item.id IN (SELECT action_item_id FROM action_item__client WHERE client_id = ${unPk client_id}) AND
                project_id = ${unPk project_id} AND
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
        action_item.id = ${unPk pk};|]
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

create :: Stored Client -> ActionItem -> Sql (Stored ActionItem)
create client item@(ActionItem{..}) = do
    ids <- query [pgSQL|
        -- FIXME add defaults in the database for the timing stuff
        INSERT INTO action_item (
            text,
            project_id,
            action_type_id,
            action_status_id,
            weight_id,
            timescale_id,
            deadline,
            behalf_of
        ) (
            SELECT
                ${text},
                ${unPk <$> project},
                rt.action_type.id,
                rt.action_status.id,
                rt.weight.id,
                rt.timescale.id,
                ${deadline},
                ${behalf_of}
            FROM rt.action_type, rt.weight, rt.timescale, rt.action_status
            WHERE
                rt.action_type.description = ${action_type} AND
                rt.action_status.description = ${action_status} AND
                rt.weight.description = ${weight} AND
                rt.timescale.description = ${timescale}
        )
        RETURNING id;|]
    item <- case ids of
        [pk] -> pure $ Stored (Pk pk) item
        _ -> error "sql insert failed"
    execute [pgSQL|
        INSERT INTO action_item__client (action_item_id, client_id)
        VALUES (${unPk $ thePk item}, ${unPk $ thePk client});|]
    pure item

update :: Stored ActionItem -> Sql (Maybe (Stored ActionItem))
update item@(Stored pk ActionItem{..}) = do
    ids <- query [pgSQL|
        UPDATE action_item SET
            text = ${text},
            project_id = ${unPk <$> project},
            action_type_id = (SELECT id from rt.action_type WHERE description = ${action_type}),
            action_status_id = (SELECT id from rt.action_status WHERE description = ${action_status}),
            weight_id = (SELECT id from rt.weight WHERE description = ${weight}),
            timescale_id = (SELECT id from rt.timescale WHERE description = ${timescale}),
            deadline = ${deadline},
            behalf_of = ${behalf_of},
            last_accessed_on = current_date,
            last_update_on = current_date
        WHERE
            action_item.id = ${unPk pk}
        RETURNING id;|]
    pure $ case ids of
        [] -> Nothing
        [pk :: Int32] -> Just item
        _ -> error "sql insert failed"


xformRow (id, text, (Pk <$>) -> project, action_type, action_status, weight, timescale, deadline, behalf_of) =
    Stored (Pk id) ActionItem{..}