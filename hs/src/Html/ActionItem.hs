{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Html.ActionItem where

import Data.Time.Calendar (Day)
import Data.Time.Format

import Control.Monad

import Util
import Data.Db
import Html

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem

full :: Monad m => Stored ActionItem -> HtmlT m () 
full (Stored pk ActionItem{..}) = do
    div_ $ do
        p_ $ do
            toHtml text
        div_ $ do
            unmaybeM_ deadline $ \deadline -> do
                let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) deadline
                small_ $ toHtml date
                " "
            small_ $ toHtml timescale
            " "
            small_ $ toHtml weight
            " "
            small_ $ toHtml action_status
            " "
            small_ $ toHtml action_type

form :: Monad m => HtmlT m ()
form = form_ ! [id_ "create-action-item", spellcheck_ "true"] $ do
    div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, autocomplete_ "off", placeholder_ "describe action item"] $ mempty
    div_ $ do
        dropdown_ (Left "select type") ["negentropy", "intel", "decision", "artifact", "learning"] ! [name_ "action_type", required_ "true"]
        dropdown_ (Left "select timescale") ["hours", "days", "weeks", "months", "years"] ! [name_ "timescale", required_ "true"]
        dropdown_ (Left "select weight") ["trivial", "minor", "medium", "major"] ! [name_ "weight", required_ "true"]
        dropdown_ (Right "queued") ["proposed", "queued", "active", "complete", "dismissed"] ! [name_ "action_status", required_ "true"]
    div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
    div_ $ do
        button_ ! [type_ "submit"] $ "Create"
        button_ ! [type_ "reset"] $ "Cancel"
