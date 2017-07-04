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
    , action_status :: Maybe Text
    , weight :: Maybe Text
    , timescale :: Maybe Text
    , deadline :: Maybe Day -- FIXME should be Maybe (Maybe Day)
    , project_id :: Maybe (Maybe (Pk Project))
    }

instance Default Form where
    def = Form
        { text = Nothing
        , action_status = Nothing
        , weight = Nothing
        , timescale = Nothing
        , deadline = Nothing
        , project_id = Nothing
        }

instance ToForm ActionItem Form where
    toForm ActionItem{..} = Form
        { text = Just text
        , action_status = Just action_status
        , weight = Just weight
        , timescale = Just timescale
        , deadline = deadline
        , project_id = Just project_id
        }

instance FromForm ActionItem Form where
    fromForm Form{..} = do
        text <- text
        action_status <- action_status
        weight <- weight
        timescale <- timescale
        project_id <- pure $ join project_id
        pure ActionItem{..}

instance PatchForm ActionItem Form where
    patchForm item Form{..} = ActionItem
        { text = fromMaybe (ActionItem.text item) text
        , action_status = fromMaybe (ActionItem.action_status item) action_status
        , weight = fromMaybe (ActionItem.weight item) weight
        , timescale = fromMaybe (ActionItem.timescale item) timescale
        , deadline = deadline <|> ActionItem.deadline item
        , project_id = fromMaybe (ActionItem.project_id item) project_id
        }