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


create :: Stored Client -> ActionItem -> Sql (Stored ActionItem)
create client item@(ActionItem{..}) = do
    ids <- query $(fileStr "ActionItem-create.sql")
                    (text, project, deadline, behalf_of, action_type, action_status, weight, timescale)
    item <- case ids of
        [Only pk] -> pure $ Stored pk item
        _ -> error "sql insert failed"
    execute "INSERT INTO action_item__client (action_item_id, client_id) VALUES (?, ?);" (thePk item, thePk client)
    pure item

update :: Stored ActionItem -> Sql (Maybe (Stored ActionItem))
update item@(Stored pk ActionItem{..}) = do
    ids <- query $(fileStr "ActionItem-update.sql")
                    (text, project, action_type, action_status, weight, timescale, deadline, behalf_of, pk)
    pure $ case ids of
        [] -> Nothing
        [Only (pk :: Pk ActionItem)] -> Just item
        _ -> error "sql insert failed"

all :: Sql [Stored ActionItem]
all = (xformRow <$>) <$> query_ $(fileStr "ActionItem-all.sql")

dashboard :: Stored Client -> Maybe (Stored Project) -> Sql [Stored ActionItem]
dashboard (thePk -> client_id) project = (xformRow <$>) <$> rows
    where
    rows = case project of
        Nothing -> query $(fileStr "ActionItem-dashboard_noProject.sql") (Only client_id)
        Just (thePk -> project_id) -> query $(fileStr "ActionItem-dashboard.sql") (client_id, project_id)

byPk :: Pk ActionItem -> Sql (Maybe (Stored ActionItem))
byPk pk = xform <$> query $(fileStr "ActionItem-byPk.sql") (Only pk)
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it


xformRow (id, text, project, action_type, action_status, weight, timescale, deadline, behalf_of) =
    Stored id ActionItem{..}