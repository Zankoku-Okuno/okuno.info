{-#LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Project where

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

