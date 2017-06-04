module Html.Client where

import ClassyPrelude

import Data.Db
import Html

import Data.Client (Client(..), Username(..))

userUrl :: Stored Client -> Text -> Text
userUrl client rest = "/~" ++ (theUName . name . thePayload $ client) ++ rest