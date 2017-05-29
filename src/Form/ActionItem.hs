{-#LANGUAGE MultiParamTypeClasses #-}
module Form.ActionItem where

import ClassyPrelude

import Data.Default
import Data.Db
import Form

import Data.Project (Project)
import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem


data Form = Form
    { text :: Maybe Text
    , project :: Maybe (Maybe (Pk Project))
    , action_type :: Maybe Text
    , action_status :: Maybe Text
    , weight :: Maybe Text
    , timescale :: Maybe Text
    , deadline :: Maybe Day -- FIXME should be Maybe (Maybe Day)
    , behalf_of :: Maybe (Maybe Text)
    }

instance Default Form where
    def = Form
        { text = Nothing
        , project = Nothing
        , action_type = Nothing
        , action_status = Nothing
        , weight = Nothing
        , timescale = Nothing
        , deadline = Nothing
        , behalf_of = Nothing
        }

instance ToForm ActionItem Form where
    toForm ActionItem{..} = Form
        { text = Just text
        , project = Just project
        , action_type = Just action_type
        , action_status = Just action_status
        , weight = Just weight
        , timescale = Just timescale
        , deadline = deadline
        , behalf_of = Just behalf_of
        }

instance FromForm ActionItem Form where
    fromForm Form{..} = do
        text <- text
        project <- project
        action_type <- action_type
        action_status <- action_status
        weight <- weight
        timescale <- timescale
        behalf_of <- behalf_of
        pure ActionItem{..}

instance PatchForm ActionItem Form where
    patchForm item Form{..} = ActionItem
        { text = fromMaybe (ActionItem.text item) text
        , project = fromMaybe (ActionItem.project item) project
        , action_type = fromMaybe (ActionItem.action_type item) action_type
        , action_status = fromMaybe (ActionItem.action_status item) action_status
        , weight = fromMaybe (ActionItem.weight item) weight
        , timescale = fromMaybe (ActionItem.timescale item) timescale
        , deadline = deadline <|> ActionItem.deadline item
        , behalf_of = fromMaybe (ActionItem.behalf_of item) behalf_of
        }