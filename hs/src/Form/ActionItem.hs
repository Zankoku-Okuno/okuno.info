{-#LANGUAGE OverloadedStrings, RecordWildCards, DuplicateRecordFields, OverloadedLabels #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Form.ActionItem where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Time.Calendar (Day)

import Util
import Data.Db
import Form

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem


data Form = Form
    { text :: Maybe Text
    , action_type :: Maybe Text
    , weight :: Maybe Text
    , timescale :: Maybe Text
    , deadline :: Maybe Day
    , action_status :: Maybe Text
    }

instance Default Form where
    def = Form
        { text = Nothing
        , action_type = Nothing
        , weight = Nothing
        , timescale = Nothing
        , deadline = Nothing
        , action_status = Nothing
        }

instance ToForm ActionItem Form where
    toForm ActionItem{..} = Form
        { text = Just text
        , action_type = Just action_type
        , weight = Just weight
        , timescale = Just timescale
        , deadline = deadline
        , action_status = Just action_status
        }

instance FromForm ActionItem Form where
    fromForm Form{..} = do
        text <- text
        action_type <- action_type
        weight <- weight
        timescale <- timescale
        action_status <- action_status
        pure ActionItem{..}

instance PatchForm ActionItem Form where
    patchForm item Form{..} = ActionItem
        { text = fromMaybe (ActionItem.text item) text
        , action_type = fromMaybe (ActionItem.action_type item) action_type
        , weight = fromMaybe (ActionItem.weight item) weight
        , timescale = fromMaybe (ActionItem.timescale item) timescale
        , deadline = deadline <|> ActionItem.deadline item
        , action_status = fromMaybe (ActionItem.action_status item) action_status
        }