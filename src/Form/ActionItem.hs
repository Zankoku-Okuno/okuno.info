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
    , lifecycle :: Maybe Text
    , weight :: Maybe Text
    , timescale :: Maybe Text
    , deadline :: Maybe (Maybe Day)
    , project_id :: Maybe (Maybe (Pk Project))
    }

instance Default Form where
    def = Form
        { text = Nothing
        , lifecycle = Nothing
        , weight = Nothing
        , timescale = Nothing
        , deadline = Nothing
        , project_id = Nothing
        }

instance ToForm ActionItem Form where
    toForm ActionItem{..} = Form
        { text = Just text
        , lifecycle = Just lifecycle
        , weight = Just weight
        , timescale = Just timescale
        , deadline = Just deadline
        , project_id = Just project_id
        }

instance FromForm ActionItem Form where
    fromForm Form{..} = do
        text <- text
        lifecycle <- lifecycle
        weight <- weight
        timescale <- timescale
        project_id <- pure $ join project_id
        deadline <- pure $ join deadline
        pure ActionItem{..}

instance PatchForm ActionItem Form where
    patchForm item Form{..} = ActionItem
        { text = fromMaybe (ActionItem.text item) text
        , lifecycle = fromMaybe (ActionItem.lifecycle item) lifecycle
        , weight = fromMaybe (ActionItem.weight item) weight
        , timescale = fromMaybe (ActionItem.timescale item) timescale
        , deadline = fromMaybe (ActionItem.deadline item) deadline
        , project_id = fromMaybe (ActionItem.project_id item) project_id
        }