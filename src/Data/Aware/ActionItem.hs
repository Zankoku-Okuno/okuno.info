module Data.Aware.ActionItem
    ( Aware(..)
    ) where

import ClassyPrelude

import Data.Db

import Data.Client (Client, Username)
import qualified Data.Client as Client
import Data.ActionItem (ActionItem)
import qualified Data.ActionItem as ActionItem
import Data.Project (Project)
import qualified Data.Project as Project
import Data.Tag (Tag(Tag))
import qualified Data.Tag as Tag


data Aware = Aware
    { client :: Stored Client
    , action_item :: Stored ActionItem
    , project :: Maybe (Stored Project)
    , tags :: [Stored Tag]
    }


byId :: (Pk ActionItem, Username) -> Sql (Maybe Aware)
byId (action_item_id, username) = do
    client <- Client.byName username
    action_item <- ActionItem.byPk action_item_id
    case (action_item, client) of
        (Just action_item, Just client) -> do
            [(Pk -> aware_id :: Pk Aware, (Pk <$>) -> project_id)] <- query [pgSQL|
                SELECT id, project_id
                FROM awareness
                WHERE
                    client_id = ${unPk $ thePk client} AND
                    action_item_id = ${unPk $ thePk action_item};|]
            project <- maybe (pure Nothing) Project.byPk project_id
            tags <- (xformTag <$>) <$> query [pgSQL|
                SELECT tag.id, name
                FROM tag
                    JOIN awareness__tag ON (tag.id = tag_id)
                    JOIN awareness ON (awareness_id = awareness.id)
                WHERE awareness.id = ${unPk aware_id};|]
            pure $ Just Aware{..}
        _ -> pure Nothing


xformTag (pk, name) = Stored (Pk pk) Tag{..}