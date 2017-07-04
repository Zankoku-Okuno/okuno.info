{-#LANGUAGE MultiParamTypeClasses #-}
module Form.ActionItem where

import ClassyPrelude

import Data.Default
import Data.Db
import Form

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import Data.Project (Project)
import Data.Tag (Tag)


data Form = Form
    { text :: Maybe Text
    , lifecycle :: Maybe Text
    , weight :: Maybe Text
    , timescale :: Maybe Text
    , deadline :: Maybe (Maybe Day)
    , project_id :: Maybe (Maybe (Pk Project))
    , tag_ids :: Maybe [Pk Tag]
    }

instance Default Form where
    def = Form
        { text = Nothing
        , lifecycle = Nothing
        , weight = Nothing
        , timescale = Nothing
        , deadline = Nothing
        , project_id = Nothing
        , tag_ids = Nothing
        }

instance ToForm ActionItem Form where
    toForm ActionItem{..} = Form
        { text = Just text
        , lifecycle = Just lifecycle
        , weight = Just weight
        , timescale = Just timescale
        , deadline = Just deadline
        , project_id = Just project_id
        , tag_ids = Just tag_ids
        }

instance FromForm ActionItem Form where
    fromForm Form{..} = do
        text <- text
        lifecycle <- lifecycle
        weight <- weight
        timescale <- timescale
        deadline <- pure $ join deadline
        project_id <- pure $ join project_id
        tag_ids <- tag_ids
        pure ActionItem{..}

instance PatchForm ActionItem Form where
    patchForm item Form{..} = ActionItem
        { text = fromMaybe (ActionItem.text item) text
        , lifecycle = fromMaybe (ActionItem.lifecycle item) lifecycle
        , weight = fromMaybe (ActionItem.weight item) weight
        , timescale = fromMaybe (ActionItem.timescale item) timescale
        , deadline = fromMaybe (ActionItem.deadline item) deadline
        , project_id = fromMaybe (ActionItem.project_id item) project_id
        , tag_ids = fromMaybe (ActionItem.tag_ids item) tag_ids
        }