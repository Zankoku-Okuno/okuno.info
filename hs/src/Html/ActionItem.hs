{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Html.ActionItem where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad

import Util
import Data.Db
import Form
import Html

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import qualified Form.ActionItem as ActionItem


full :: Monad m => Stored ActionItem -> HtmlT m () 
full item@(Stored pk ActionItem{..}) = do
    let tabset = T.concat ["action_item-", T.pack $ show pk]
    select_ [data_ "tabset" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        p_ $ do
            toHtml text
        div_ $ do
            maybeM_ deadline $ \deadline -> do
                let date = showTime deadline
                small_ $ toHtml date
                " "
            small_ $ toHtml timescale
            " "
            small_ $ toHtml weight
            " "
            small_ $ toHtml action_status
            " "
            small_ $ toHtml action_type
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ form $
        (first Just) (toForm item)

form :: Monad m => (Maybe (Pk ActionItem), ActionItem.Form) -> HtmlT m ()
form (pk, ActionItem.Form{..}) = form_ ! [method_ "PUT", action_ "/action-item", spellcheck_ "true"] $ do
    maybeM_ pk $ \pk ->
        input_ [type_ "hidden", name_ "id", value_ $ (T.pack . show) pk]
    
    div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, autocomplete_ "off", placeholder_ "describe action item"] $
        maybeM_ text toHtml

    div_ $ do
        dropdown_ (maybe (Left "select type") Right action_type) action_type_opts ! [name_ "action_type", required_ "true"]
        dropdown_ (maybe (Left "select timescale") Right timescale) timescale_opts ! [name_ "timescale", required_ "true"]
        dropdown_ (maybe (Left "select weight") Right weight) weight_opts ! [name_ "weight", required_ "true"]
        dropdown_ (maybe (Right "queued") Right action_status) action_status_opts ! [name_ "action_status", required_ "true"]
    div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
            ! maybe [] ((:[]) . value_ . T.pack . showTime) deadline
    div_ $ do
        button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
        button_ ! [type_ "reset"] $ "Cancel"
    where
    action_type_opts = ["negentropy", "intel", "decision", "artifact", "learning"]
    timescale_opts = ["hours", "days", "weeks", "months", "years"]
    weight_opts = ["trivial", "minor", "medium", "major"]
    action_status_opts = ["proposed", "queued", "active", "complete", "dismissed"]
