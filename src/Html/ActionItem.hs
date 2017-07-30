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


full :: (Day, ([Stored Project], [Stored Tag])) -> ActionItem.Loaded -> Html ()
full (today, options) loaded@(client, item@(Stored pk ActionItem{..}), project, tags) = do
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
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form client options (Just loaded)
            ! [ autocomplete_ "off" ]

-- FIXME there should be a LoadedForm for action items that should be sufficient to handle this
form :: Stored Client -> ([Stored Project], [Stored Tag]) -> Maybe ActionItem.Loaded -> Html ()
form client (allProjects, allTags) item_m = case item_m of
    Nothing -> super $ do
        div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, placeholder_ "describe action item"] $
            pure ()
        div_ [class_ "meta "] $ do
            div_ $ do
                dropdown_ (Left "select timescale") RT.timescale ! [name_ "timescale", required_ "true"]
                dropdown_ (Left "select weight")    RT.weight    ! [name_ "weight", required_ "true"]
                dropdown_ (Right "queued")          RT.lifecycle ! [name_ "lifecycle", required_ "true"]
            select_ ! [name_ "project"] $ do
                option_ ! [value_ "", selected_ "true"] $ "unassigned"
                forM_ allProjects $ \(Stored pk Project{..}) -> do
                    option_ ! [value_ $ tshow pk] $ toHtml name
            let tagset = "action_item_tags_new"
            div_ ! [class_ "tags"] $ do
                select_ ! [multiple_ "true", name_ "tag"] $ do
                    forM_ allTags $ \(Stored pk Tag{..}) -> do
                        option_ ! [value_ $ tshow pk] $ toHtml name
            div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
        div_ $ do
            button_ ! [type_ "submit"] $ "Create"
            button_ ! [type_ "reset"] $ "Cancel"
    Just (_, Stored pk ActionItem{..}, project, tags) -> super $ do
        input_ [type_ "hidden", name_ "id", value_ $ tshow pk]
        div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, placeholder_ "describe action item"] $
            toHtml text
        div_ [class_ "meta "] $ do
            div_ $ do
                dropdown_ (Right timescale) RT.timescale ! [name_ "timescale", required_ "true"]
                dropdown_ (Right weight)    RT.weight    ! [name_ "weight", required_ "true"]
                dropdown_ (Right lifecycle) RT.lifecycle ! [name_ "lifecycle", required_ "true"]
            select_ ! [name_ "project"] $ do
                option_ ! [value_ ""] ! maybe [] (const [selected_ "true"]) project $ "unassigned"
                forM_ allProjects $ \(Stored pk Project{..}) -> do
                    option_ ! [value_ $ tshow pk] ! (if (thePk <$> project) == Just pk then [selected_ "true"] else []) $ toHtml name
            let tagset = concat ["action_item_tags_", tshow pk]
            div_ ! [class_ "tags"] $ do
                select_ ! [multiple_ "true", name_ "tag"] $ do
                    forM_ allTags $ \(Stored pk Tag{..}) -> do
                        option_ ! [value_ $ tshow pk]
                                ! (if pk `elem` (thePk <$> tags) then [selected_ "true"] else [])
                                $ toHtml name
                -- TODO template for new tags
                -- TODO dropdown do make it pretty, with spot to show tags
            div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
                    ! maybe [] ((:[]) . value_ . pack . showTime) deadline
        div_ $ do
            button_ ! [type_ "submit"] $ "Save"
            button_ ! [type_ "reset"] $ "Cancel"
    where
    super body =
        form_ ! [ method_ "PUT"
                , action_ $ userUrl client "/action-item"
                , class_ "action_item "
                , spellcheck_ "true"
                ] $ body