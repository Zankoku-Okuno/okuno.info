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
    , lifecycle :: Maybe Text
    }

instance Default Form where
    def = Form
        { name = Nothing
        , mission = Nothing
        , lifecycle = Nothing
        }

instance ToForm Project Form where
    toForm Project{..} = Form
        { name = Just name
        , mission = Just mission
        , lifecycle = Just lifecycle
        }

instance FromForm Project Form where
    fromForm Form{..} = do
        name <- name
        mission <- mission
        lifecycle <- lifecycle
        pure Project{..}

instance PatchForm Project Form where
    patchForm item Form{..} = Project
        { name = fromMaybe (Project.name item) name
        , mission = fromMaybe (Project.mission item) mission
        , lifecycle = fromMaybe (Project.lifecycle item) lifecycle
        }