module Data.Client
    ( Client(..)
    , Username(..)
    , byName
    ) where

import ClassyPrelude
import Data.Db


newtype Username = Username { theUName :: Text }
    deriving (Eq, Show)
instance IsString Username where
    fromString = Username . fromString

data Client = Client
    { name :: Username
    , email :: Text
    }


byName :: Username -> Sql (Maybe (Stored Client))
byName (theUName -> name) = xform <$> query "SELECT id, name, email FROM client WHERE name = ?;" (Only name)
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it


xformRow (id, Username -> name, email) = Stored id Client{..}