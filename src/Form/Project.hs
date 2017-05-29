{-#LANGUAGE MultiParamTypeClasses #-}
module Form.Project where

import ClassyPrelude

import Data.Default
import Data.Db
import Form

import Data.Project (Project(..))
import qualified Data.Project as Project


data Form = Form
    { name :: Maybe Text
    , mission :: Maybe Text
    , action_type :: Maybe Text
    , action_status :: Maybe Text
    }

instance Default Form where
    def = Form
        { name = Nothing
        , mission = Nothing
        , action_type = Nothing
        , action_status = Nothing
        }

instance ToForm Project Form where
    toForm Project{..} = Form
        { name = Just name
        , mission = Just mission
        , action_type = Just action_type
        , action_status = Just action_status
        }

instance FromForm Project Form where
    fromForm Form{..} = do
        name <- name
        mission <- mission
        action_type <- action_type
        action_status <- action_status
        pure Project{..}

instance PatchForm Project Form where
    patchForm item Form{..} = Project
        { name = fromMaybe (Project.name item) name
        , mission = fromMaybe (Project.mission item) mission
        , action_type = fromMaybe (Project.action_type item) action_type
        , action_status = fromMaybe (Project.action_status item) action_status
        }