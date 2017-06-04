module Data.Client
    ( Client(..)
    , byName
    ) where

import ClassyPrelude
import Data.Db


data Client = Client
    { name :: Text
    , email :: Text
    }


byName :: Text -> Sql (Maybe (Stored Client))
byName name = xform <$> query "SELECT id, name, email FROM client WHERE name = ?;" (Only name)
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it


xformRow (id, name, email) = Stored id Client{..}