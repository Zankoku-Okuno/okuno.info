{-#LANGUAGE MultiParamTypeClasses #-}
module Form.Tag where

import ClassyPrelude

import Data.Default
import Data.Db
import Form

import Data.Tag (Tag(..))
import qualified Data.Tag as Tag


data Form = Form
    { name :: Maybe Text
    }

instance Default Form where
    def = Form
        { name = Nothing
        }

instance ToForm Tag Form where
    toForm Tag{..} = Form
        { name = Just name
        }

instance FromForm Tag Form where
    fromForm Form{..} = do
        name <- name
        pure Tag{..}

instance PatchForm Tag Form where
    patchForm item Form{..} = Tag
        { name = fromMaybe (Tag.name item) name
        }