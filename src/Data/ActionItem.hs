{-# LANGUAGE TemplateHaskell #-}
module Data.ActionItem
    ( ActionItem(..)
    , Loaded, load
    , dashboard
    , byPk
    , create, update
    , options -- FIXME choose a better name and location
    ) where

import ClassyPrelude
import Util (maybeM_)
import Data.Db

import Data.Project (Project)
import qualified Data.Project as Project
import Data.Tag (Tag)
import qualified Data.Tag as Tag
import Data.Client (Client, Username)


data ActionItem = ActionItem
    { text :: Text
    , lifecycle :: Text
    , weight :: Text
    , timescale :: Text
    , deadline :: Maybe Day
    , project_id :: Maybe (Pk Project)
    , tag_ids :: [Pk Tag]
    } deriving (Show)

type Loaded = (Stored Client, Stored ActionItem, Maybe (Stored Project), [Stored Tag])
load :: Stored Client -> Stored ActionItem -> Sql Loaded
load client item@(thePayload -> ActionItem{..}) = do
    project <- case project_id of
        Nothing -> pure Nothing
        Just project_id -> Project.byPk project_id
    tags <- catMaybes <$> (Tag.byId `mapM` tag_ids)
    pure (client, item, project, tags)

dashboard :: Stored Client -> Maybe (Stored Project) -> Sql [Stored ActionItem]
dashboard client project = do
    preitems <- (xformRow <$>) <$> query [pgSQL|
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
                WHERE
                    CASE WHEN ${unPk . thePk <$> project}::INTEGER IS NULL
                        THEN aware_action_item.project_id IS NULL
                        ELSE aware_action_item.project_id = ${fromMaybe (negate 1) $ unPk . thePk <$> project}
                    END
            ) AND
            rt.lifecycle.description IN ('proposed', 'queued', 'waiting', 'active')
        ORDER BY
            rt.lifecycle.description = 'active' DESC,
            rt.lifecycle.description = 'waiting' DESC,
            rt.lifecycle.description = 'queued' DESC,
            value_for_time + crunch + random() DESC,
            last_accessed_on DESC;|]
    _loadTags client `mapM` preitems

byPk :: (Stored Client, Pk ActionItem) -> Sql (Maybe (Stored ActionItem))
byPk (client, action_item_id) = do
    preitem <- xform <$> query [pgSQL|
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
    case preitem of
        Nothing -> pure Nothing
        Just preitem -> Just <$> _loadTags client preitem
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

_loadTags :: Stored Client -> Stored ActionItem -> Sql (Stored ActionItem)
_loadTags client (Stored pk preitem) = do
    tag_ids <- (Pk <$>) <$> query [pgSQL|
        SELECT tag_id
        FROM awareness__tag
            JOIN aware_action_item ON (aware_action_item.id = awareness_id)
            JOIN action_item ON (action_item.id = action_item_id)
        WHERE
            aware_action_item.client_id = ${unPk $ thePk client} AND
            action_item.id = ${unPk pk};|]
    pure . Stored pk $ preitem {tag_ids = tag_ids}

create :: (Stored Client, ActionItem) -> Sql (Stored ActionItem)
create (client, item@ActionItem{..}) = do
    ids <- query [pgSQL|
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
    ids <- query [pgSQL|
        INSERT INTO aware_action_item (
            action_item_id,
            client_id,
            project_id
        ) VALUES (
            ${unPk $ thePk item},
            ${unPk $ thePk client},
            ${unPk <$> project_id}
        )
        RETURNING id;|]
    aware <- case ids of
        [pk] -> pure (pk :: Int32)
        _ -> error "sql insert failed"
    forM_ tag_ids $ \tag_id -> void $ execute [pgSQL|
        INSERT INTO awareness__tag (
            awareness_id,
            tag_id
        ) VALUES (
            ${aware},
            ${unPk tag_id}
        );|]
    pure item

update :: (Stored Client, Pk ActionItem) -> ActionItem -> Sql (Maybe (Stored ActionItem))
update (client, pk) item@ActionItem{..} = do
    ids <- query [pgSQL|
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
    item <- case ids of
        [] -> pure Nothing
        [pk] -> pure . Just $ Stored (Pk pk) item
        _ -> error "sql insert failed"
    maybeM_ item $ \item -> do
        ids <- query [pgSQL|
            UPDATE aware_action_item SET
                project_id = ${unPk <$> project_id}
            WHERE
                action_item_id = ${unPk pk} AND
                client_id = ${unPk $ thePk client}
            RETURNING id;|]
        aware <- case ids of
            [pk] -> pure (pk :: Int32)
            _ -> error "sql insert failed"
        void $ execute [pgSQL|
            DELETE FROM awareness__tag
            WHERE awareness_id = ${aware};|]
        forM_ tag_ids $ \tag_id -> void $ execute [pgSQL|
            INSERT INTO awareness__tag (
                awareness_id,
                tag_id
            ) VALUES (
                ${aware},
                ${unPk tag_id}
            );|]
    pure item


xformRow :: (Int32, Maybe Int32, Text, Text, Text, Text, Maybe Day) -> Stored ActionItem
xformRow (id, (Pk <$>) -> project_id, text, lifecycle, weight, timescale, deadline) =
    Stored (Pk id) ActionItem{tag_ids = [], ..}



options :: Stored Client -> Sql ([Stored Project], [Stored Tag])
options client = do
    projects <- Project.byClient client
    tags <- Tag.byClient client
    pure (projects, tags)
