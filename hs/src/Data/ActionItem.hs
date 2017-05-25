{-#LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.ActionItem where

-- bytes/text
import qualified Data.Text as T
import Data.Text (Text)
import Data.String (IsString(..))

import Data.Time.Calendar
import Data.Time.Clock
import Data.Db
import Data.Project

import Util


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


create :: ActionItem -> Sql (Stored ActionItem)
create item@(ActionItem{..}) = do
    ids <- query $(fileStr "ActionItem-create.sql")
                    (text, project, deadline, behalf_of, action_type, action_status, weight, timescale)
    case ids of
        [Only pk] -> pure $ Stored pk item
        _ -> error "sql insert failed"

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
    where
    xformRow (id, text, project, action_type, action_status, weight, timescale, deadline, behalf_of) =
        Stored id ActionItem{..}

byPk :: Pk ActionItem -> Sql (Maybe (Stored ActionItem))
byPk pk = xform <$> query $(fileStr "ActionItem-byPk.sql") (Only pk)
    where
    xform [] = Nothing
    xform [(id, text, project, action_type, action_status, weight, timescale, deadline, behalf_of)] =
        Just $ Stored id ActionItem{..}