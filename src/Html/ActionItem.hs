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
import Data.Tag (Tag(..))
import Data.Client (Client(..), Username(..))
import Html.Client


full :: Monad m => (Day, ([Stored Project], [Stored Tag])) -> ActionItem.Loaded -> HtmlT m ()
full (today, options) (client, item@(Stored pk ActionItem{..}), project, tags) = do
    let tabset = concat ["action_item_", tshow pk]
    select_ [data_ "tabs" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        div_ ! [class_ "markdown "] $ toHtml text
        div_ ! [class_ "meta "] $ do
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
            span_ ! [data_ "lifecycle" lifecycle] $ toHtml lifecycle
            maybeM_ project $ \project@(thePayload -> Project{..}) -> do
                div_ ! [data_ "project" $ tshow (thePk project)] $ toHtml name
            div_ ! [class_ "tags"] $ do
                forM_ tags $ \tag@(thePayload -> Tag{..}) -> do
                    span_ ! [class_ "tag"] $ toHtml name
            -- TODO display tags
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form client options (first Just $ toForm item)
            ! [ autocomplete_ "off" ]

form :: Monad m => Stored Client -> ([Stored Project], [Stored Tag]) -> (Maybe (Pk ActionItem), ActionItem.Form) -> HtmlT m ()
form client (projects, tags) (pk, ActionItem.Form{..}) = do
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
            div_ $ do
                dropdown_ (maybe (Left "select timescale") Right timescale) RT.timescale ! [name_ "timescale", required_ "true"]
                dropdown_ (maybe (Left "select weight") Right weight) RT.weight ! [name_ "weight", required_ "true"]
                dropdown_ (maybe (Right "queued") Right lifecycle) RT.lifecycle ! [name_ "lifecycle", required_ "true"]
            select_ ! [name_ "project"] $ do
                option_ ! [value_ ""] ! maybe [] (const [selected_ "true"]) (join project_id) $ "unassigned"
                forM_ projects $ \(Stored pk Project{..}) -> do
                    option_ ! [value_ $ tshow pk] ! (if join project_id == Just pk then [selected_ "true"] else []) $ toHtml name
            let tagset = concat ["action_item_tags_", maybe "new" tshow pk]
            div_ $ do
                select_ ! [data_ "multi_select" tagset] $ do
                    option_ ! [value_ "", selected_ "true", disabled_ "true"] $ "add tags"
                    forM_ tags $ \(Stored pk Tag{..}) -> do
                        option_ ! [value_ $ tshow pk] $ toHtml name
                    option_ [value_ "new"] $ "new tag..."
                div_ ! [class_ "accum ", data_ "multi_select" tagset] $ do
                    template_ ! [class_ "multi_select_add "] $ do
                        div_ $ do
                            input_ [type_ "hidden", name_ "tag", class_ "multi_select_value "]
                            span_ ! [class_ "multi_select_label "] $ pure ()
                            " "
                            a_ ! [class_ "multi_select_delete "] $ "x"
                    template_ ! [class_ "multi_select_new "] $ do
                        div_ $ do
                            input_ [type_ "text", name_ "new_tag", placeholder_ "new tag"]
                            " "
                            a_ ! [class_ "multi_select_delete "] $ "x"
                    -- TODO present current tags
                    pure ()
            div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
                    ! maybe [] ((:[]) . value_ . pack . showTime) (join deadline)
        div_ $ do
            button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
            button_ ! [type_ "reset"] $ "Cancel"
