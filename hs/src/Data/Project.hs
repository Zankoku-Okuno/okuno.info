{-#LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Project
    ( Project(..)
    , Data.Project.all, byPk
    , create, update
    ) where

-- bytes/text
import qualified Data.Text as T
import Data.Text (Text)
import Data.String (IsString(..))

import Data.Db

import Util


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

create :: Project -> Sql (Stored Project)
create project@(Project{..}) = do
    ids <- query $(fileStr "Project-create.sql")
            (name, mission, action_type, action_status)
    case ids of
        [Only pk] -> pure $ Stored pk project
        _ -> error "sql insert failed"

update :: Stored Project -> Sql (Maybe (Stored Project))
update item@(Stored pk Project{..}) = do
    ids <- query $(fileStr "Project-update.sql")
                    (name, mission, action_type, action_status, pk)
    pure $ case ids of
        [] -> Nothing
        [Only (pk :: Pk Project)] -> Just item
        _ -> error "sql insert failed"

xformRow (id, name, mission, action_type, action_status) = Stored (Pk id) $ Project{..}