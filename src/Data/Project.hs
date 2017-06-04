{-# LANGUAGE TemplateHaskell #-}
module Data.Project
    ( Project(..)
    , Data.Project.all, byClient
    , byPk
    , create, update
    ) where

import ClassyPrelude

import Data.Db
import Data.Client (Client)
import Util (fileStr)


data Project = Project
    { name :: Text
    , mission :: Text
    , action_type :: Text
    , action_status :: Text
    } deriving (Show)

all :: Sql [Stored Project]
all = (xformRow <$>) <$> query_ $(fileStr "Project-all.sql")

byPk :: Pk Project -> Sql (Maybe (Stored Project))
byPk pk = xform <$> query $(fileStr "Project-byPk.sql") (Only pk)
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

byClient :: Stored Client -> Sql [Stored Project]
byClient (Stored pk _) = (xformRow <$>) <$> query $(fileStr "Project-byClient.sql") (Only pk)

create :: Stored Client -> Project -> Sql (Stored Project)
create client project@(Project{..}) = do
    ids <- query $(fileStr "Project-create.sql")
            (name, mission, action_type, action_status)
    project <- case ids of
        [Only pk] -> pure $ Stored pk project
        _ -> error "sql insert failed"
    execute "INSERT INTO project__client (project_id, client_id) VALUES (?, ?);" (thePk project, thePk client)
    pure project

update :: Stored Project -> Sql (Maybe (Stored Project))
update item@(Stored pk Project{..}) = do
    ids <- query $(fileStr "Project-update.sql")
                    (name, mission, action_type, action_status, pk)
    pure $ case ids of
        [] -> Nothing
        [Only (pk :: Pk Project)] -> Just item
        _ -> error "sql insert failed"

xformRow (id, name, mission, action_type, action_status) = Stored (Pk id) $ Project{..}