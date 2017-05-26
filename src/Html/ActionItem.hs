{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, MultiWayIf #-}
module Html.ActionItem where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Time (Day)

import Util
import Data.Db
import Form
import Html

import qualified Data.RefTables as RT
import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import qualified Form.ActionItem as ActionItem
import Data.Project (Project(..))


full :: Monad m => (Day, [Stored Project]) -> Stored ActionItem -> HtmlT m ()
full (today, projects) item@(Stored pk ActionItem{..}) = do
    let tabset = T.concat ["action_item-", T.pack $ show pk]
    select_ [data_ "tabs" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        p_ $ do
            toHtml text
        div_ $ do
            maybeM_ deadline $ \deadline -> do
                let time_status = if | deadline < today -> "overdue"
                                     | deadline == today -> "today"
                                     -- TODO if the deadline is within the time interval
                                     | otherwise -> "normal"
                    date = showTime deadline
                small_ ! [class_ "action_item_meta ", data_ "time_status" time_status]$ toHtml date
                " "
            small_ ! [class_ "action_item_meta ", data_ "timescale" timescale] $ toHtml timescale
            " "
            small_ ! [class_ "action_item_meta ", data_ "weight" weight] $ toHtml weight
            " "
            small_ ! [class_ "action_item_meta ", data_ "action_status" action_status] $ toHtml action_status
            " "
            small_ ! [class_ "action_item_meta ", data_ "action_type" action_type] $ toHtml action_type
            maybeM_ behalf_of $ \behalf_of -> do
                " "
                small_ ! [class_ "action_item_behalf_of", data_ "behalf_of" behalf_of] $ toHtml behalf_of
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form projects (first Just $ toForm item) ! [autocomplete_ "off"]

form :: Monad m => [Stored Project] -> (Maybe (Pk ActionItem), ActionItem.Form) -> HtmlT m ()
form projects (pk, ActionItem.Form{..}) = form_ ! [ method_ "PUT", action_ "/action-item", spellcheck_ "true"] $ do
    maybeM_ pk $ \pk ->
        input_ [type_ "hidden", name_ "id", value_ $ (T.pack . show) pk]

    div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, placeholder_ "describe action item"] $
        maybeM_ text toHtml
    div_ $ do
        dropdown_ (maybe (Left "select type") Right action_type) RT.action_type ! [name_ "action_type", required_ "true"]
        dropdown_ (maybe (Left "select timescale") Right timescale) RT.timescale ! [name_ "timescale", required_ "true"]
        dropdown_ (maybe (Left "select weight") Right weight) RT.weight ! [name_ "weight", required_ "true"]
        dropdown_ (maybe (Right "queued") Right action_status) RT.action_status ! [name_ "action_status", required_ "true"]
    select_ ! [name_ "project"] $ do
        option_ ! [value_ ""] ! maybe [] (const [selected_ "true"]) (join project) $ "unassigned"
        forM_ projects $ \(Stored pk Project{..}) -> do
            option_ ! [value_ $ (T.pack . show) pk] ! (if (join project) == (Just pk) then [selected_ "true"] else []) $ toHtml name
    div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
            ! maybe [] ((:[]) . value_ . T.pack . showTime) deadline
    div_ $ input_ [type_ "text", name_ "behalf_of", placeholder_ "on behalf of"]
            ! case behalf_of of
                Nothing -> []
                Just Nothing -> [value_ ""]
                Just (Just behalf_of) -> [value_ behalf_of]
    div_ $ do
        button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
        button_ ! [type_ "reset"] $ "Cancel"
