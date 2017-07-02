module Html.Client where

import ClassyPrelude

import Data.Db
import Html

import Data.Client (Client(..), Username(..))

userUrl :: Stored Client -> Text -> Text
userUrl client rest = "/~" ++ (theUName . name . thePayload $ client) ++ rest


navigation :: Monad m => Stored Client -> HtmlT m ()
navigation client = div_ ! [ class_ "nav "] $ do
    ul_ $ do
        li_ $ a_ ! [href_ $ userUrl client ""] $ "Dashboard"
        li_ $ a_ ! [href_ $ userUrl client "/projects"] $ "Projects"
        li_ $ a_ ! [href_ $ userUrl client "/tags"] $ "Tags"