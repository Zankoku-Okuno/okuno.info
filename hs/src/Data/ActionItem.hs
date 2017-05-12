{-#LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.ActionItem where

-- bytes/text
import qualified Data.ByteString as BS
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


create :: ActionItem -> Db [Pk ActionItem]
create ActionItem{..} = do
    ids <- query q (text, deadline, action_type, weight, timescale, action_status)
    pure $ fromOnly <$> ids
    where q = $(fileStr "ActionItem-create.sql")

-- TODO give an order to these
all :: Db [Stored ActionItem]
all = (parse <$>) <$> query_ q
    where
    q = $(fileStr "ActionItem-all.sql")
    parse (id, text, action_type, weight, timescale, deadline, action_status) =
        Stored id $ ActionItem {..}
