module Html.Project where

import ClassyPrelude

import Util (maybeM_)
import Data.Db
import Form
import Html

import qualified Data.RefTables as RT
import Data.Project (Project(..))
import qualified Data.Project as Project
import qualified Form.Project as Project
import Data.Client (Client(..))
import Html.Client (userUrl)


full :: Monad m => Stored Client -> Stored Project -> HtmlT m ()
full client project@(Stored pk Project{..}) = do
    let tabset = concat ["project_", tshow pk]
    select_ [data_ "tabs" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        div_ ! [class_ "markdown "] $ toHtml name
        p_ ! [class_ "mission "] $ toHtml mission
        div_ ! [class_ "meta "]$ do
            span_ ! [data_ "action_status" action_status] $ toHtml action_status
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form client (first Just $ toForm project) ! [autocomplete_ "off"]

form :: Monad m => Stored Client -> (Maybe (Pk Project), Project.Form) -> HtmlT m ()
form client (pk, Project.Form{..}) = do
    form_ ! [ method_ "PUT"
            , action_ $ userUrl client "/project"
            , class_ "project "
            , spellcheck_ "true"] $ do
        maybeM_ pk $ \pk ->
            input_ [type_ "hidden", name_ "id", value_ $ tshow pk]
        
        div_ $ input_ [type_ "text", name_ "name", required_ "true", autofocus_, placeholder_ "name"]
                      ! maybe [] ((:[]) . value_) name
        div_ $ textarea_ ! [name_ "mission", required_ "true", placeholder_ "mission statement"] $
            maybeM_ mission toHtml

        div_ $ do
            dropdown_ (maybe (Right "queued") Right action_status) RT.action_status ! [name_ "action_status", required_ "true"]

        div_ $ do
            button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
            button_ ! [type_ "reset"] $ "Cancel"
