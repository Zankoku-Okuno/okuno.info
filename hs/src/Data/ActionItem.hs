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


create :: ActionItem -> Sql [Pk ActionItem]
create ActionItem{..} = do
    ids <- query $(fileStr "ActionItem-create.sql")
                    (text, deadline, action_type, weight, timescale, action_status)
    pure $ fromOnly <$> ids

all :: Sql [Stored ActionItem]
all = (xformRow <$>) <$> query_ $(fileStr "ActionItem-all.sql")
    where
    xformRow (id, text, action_type, weight, timescale, deadline, action_status) =
        Stored id $ ActionItem {..}
