module Html.ActionItem where

import ClassyPrelude

import Util (maybeM_, showTime)
import Data.Db
import Form
import Html

import qualified Data.RefTables as RT
import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import qualified Form.ActionItem as ActionItem
import Data.Project (Project(..))
import Data.Client (Client(..), Username(..))
import Html.Client


full :: Monad m => (Day, Stored Client, [Stored Project]) -> Stored ActionItem -> HtmlT m ()
full (today, client, projects) item@(Stored pk ActionItem{..}) = do
    let tabset = concat ["action_item_", tshow pk]
    select_ [data_ "tabs" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        div_ ! [class_ "markdown "] $ toHtml text
        div_ ! [class_ "meta "] $ do
            -- TODO display project & tags
            maybeM_ deadline $ \deadline -> do
                let time_status = if | deadline < today -> "overdue"
                                     | deadline == today -> "today"
                                     -- TODO if the deadline is within the time interval
                                     | otherwise -> "normal"
                    date = showTime deadline
                span_ ! [data_ "time_status" time_status] $ toHtml date
                " "
            span_ ! [data_ "timescale" timescale] $ toHtml timescale
            " "
            span_ ! [data_ "weight" weight] $ toHtml weight
            " "
            span_ ! [data_ "action_status" action_status] $ toHtml action_status
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form (client, projects) (first Just $ toForm item)
            ! [ autocomplete_ "off" ]

form :: Monad m => (Stored Client, [Stored Project]) -> (Maybe (Pk ActionItem), ActionItem.Form) -> HtmlT m ()
form (client, projects) (pk, ActionItem.Form{..}) = do
    form_ ! [ method_ "PUT"
            , action_ $ userUrl client "/action-item"
            , class_ "action_item "
            , spellcheck_ "true"
            ] $ do
        maybeM_ pk $ \pk ->
            input_ [type_ "hidden", name_ "id", value_ $ tshow pk]

        div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, placeholder_ "describe action item"] $
            maybeM_ text toHtml
        div_ [class_ "meta "] $ do
            -- TODO select project
            -- select_ ! [name_ "project"] $ do
            --     option_ ! [value_ ""] ! maybe [] (const [selected_ "true"]) (join project) $ "unassigned"
            --     forM_ projects $ \(Stored pk Project{..}) -> do
            --         option_ ! [value_ $ tshow pk] ! (if (join project) == (Just pk) then [selected_ "true"] else []) $ toHtml name
            div_ $ do
                dropdown_ (maybe (Left "select timescale") Right timescale) RT.timescale ! [name_ "timescale", required_ "true"]
                dropdown_ (maybe (Left "select weight") Right weight) RT.weight ! [name_ "weight", required_ "true"]
                dropdown_ (maybe (Right "queued") Right action_status) RT.action_status ! [name_ "action_status", required_ "true"]
            div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
                    ! maybe [] ((:[]) . value_ . pack . showTime) deadline
        div_ $ do
            button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
            button_ ! [type_ "reset"] $ "Cancel"
