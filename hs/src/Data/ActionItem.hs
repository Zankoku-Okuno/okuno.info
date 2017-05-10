{-#LANGUAGE RecordWildCards, OverloadedStrings #-}
module Data.ActionItem where

-- bytes/text
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.String (IsString(..))

import Data.Time.Calendar
import Data.Time.Clock
import Data.Db


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
    where q =
            -- FIXME add defaults in the database for the timing stuff
            "INSERT INTO action_item (\
            \    text,\
            \    action_type_id,\
            \    weight_id,\
            \    timescale_id,\
            \    deadline,\
            \    action_status_id,\
            \    created_on,\
            \    last_accessed_on,\
            \    last_update_on\
            \) (\
            \   SELECT\
            \        ?,\
            \        rt.action_type.id,\
            \        rt.weight.id,\
            \        rt.timescale.id,\
            \        ?,\
            \        rt.action_status.id,\
            \        current_date,\
            \        current_date,\
            \        current_date\
            \    FROM rt.action_type, rt.weight, rt.timescale, rt.action_status\
            \    WHERE\
            \        rt.action_type.description = ? AND\
            \        rt.weight.description = ? AND\
            \        rt.timescale.description = ? AND\
            \        rt.action_status.description = ?\
            \)\
            \RETURNING id;"

-- TODO give an order to these
all :: Db [Stored ActionItem]
all = (parse <$>) <$> query_ q
    where
    q = "SELECT \
        \   action_item.id, \
        \   text, \
        \   rt.action_type.description, \
        \   rt.weight.description, \
        \   rt.timescale.description, \
        \   deadline, \
        \   rt.action_status.description \
        \FROM action_item \
        \   JOIN rt.action_type ON (action_type_id = action_type.id) \
        \   JOIN rt.weight ON (weight_id = weight.id) \
        \   JOIN rt.timescale ON (timescale_id = timescale.id) \
        \   JOIN rt.action_status ON (action_status_id = action_status.id) \
        \;"
    parse (id, text, action_type, weight, timescale, deadline, action_status) =
        Stored id $ ActionItem {..}
