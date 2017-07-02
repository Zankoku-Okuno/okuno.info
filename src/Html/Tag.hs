module Html.Tag
    ( full
    , form
    ) where

import ClassyPrelude

import Util (maybeM_)
import Data.Db
import Form
import Html

import Data.Tag (Tag(..))
import qualified Data.Tag as Tag
import qualified Form.Tag as Tag
import Data.Client (Client(..))
import Html.Client (userUrl)


full :: Monad m => Stored Client -> Stored Tag -> HtmlT m ()
full client tag@(Stored pk Tag{..}) = do
    let tabset = concat ["tag_", tshow pk]
    select_ [data_ "tabs" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        div_ $ toHtml name
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form client (first Just $ toForm tag) ! [autocomplete_ "off"]

form :: Monad m => Stored Client -> (Maybe (Pk Tag), Tag.Form) -> HtmlT m ()
form client (pk, Tag.Form{..}) = do
    form_ ! [ method_ "PUT"
            , action_ $ userUrl client "/tag"
            , class_ "tag "
            ] $ do
        maybeM_ pk $ \pk -> do
            input_ [type_ "hidden", name_ "id", value_ $ tshow pk]

        div_ $ input_ [type_ "text", name_ "name", required_ "true", autofocus_, placeholder_ "name"]
                      ! maybe [] ((:[]) . value_) name

        div_ $ do
            button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
            button_ ! [type_ "reset"] $ "Cancel"
