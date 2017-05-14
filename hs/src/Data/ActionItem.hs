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

import Util


data ActionItem = ActionItem
    { text :: Text
    , action_type :: Text
    , weight :: Text
    , timescale :: Text
    , deadline :: Maybe Day
    , action_status :: Text
    } deriving (Show)


create :: ActionItem -> Sql (Stored ActionItem)
create item@(ActionItem{..}) = do
    ids <- query $(fileStr "ActionItem-create.sql")
                    (text, deadline, action_type, weight, timescale, action_status)
    case ids of
        [Only pk] -> pure $ Stored pk item
        _ -> error "sql insert failed"

update :: Stored ActionItem -> Sql (Maybe (Stored ActionItem))
update item@(Stored pk ActionItem{..}) = do
    ids <- query $(fileStr "ActionItem-update.sql")
                    (text, action_type, weight, timescale, action_status, deadline, pk)
    pure $ case ids of
        [] -> Nothing
        [Only (pk :: Pk ActionItem)] -> Just item
        _ -> error "sql insert failed"

all :: Sql [Stored ActionItem]
all = (xformRow <$>) <$> query_ $(fileStr "ActionItem-all.sql")
    where
    xformRow (id, text, action_type, weight, timescale, deadline, action_status) =
        Stored id ActionItem{..}

byPk :: Pk ActionItem -> Sql (Maybe (Stored ActionItem))
byPk pk = xform <$> query $(fileStr "ActionItem-byPk.sql") (Only pk)
    where
    xform [] = Nothing
    xform [(id, text, action_type, weight, timescale, deadline, action_status)] =
        Just $ Stored id ActionItem{..}