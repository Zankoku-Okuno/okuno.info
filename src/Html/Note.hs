module Html.Note where

import ClassyPrelude

import Util (maybeM_)
import Data.Db
import Form
import Html

import Data.Note (Note(..), NoteState(..))
import qualified Data.Note as Note
import qualified Form.Note as Note
import Data.Client (Client(..), Username(..))
import Html.Client (userUrl)


full :: (Monad m) => Stored Client -> Stored Note -> HtmlT m ()
full client note@(Stored pk Note{..}) = do
    let tabset = concat ["note_", tshow pk]
    select_ [data_ "tabs" tabset, autocomplete_ "off"] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        div_ ! [class_ "markdown "] $ toHtml text
        div_ ! [class_ "meta "] $ do
            span_ ! [data_ "status" (pack $ show review_status)] $ toHtml (show review_status)
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "edit"
           ] $ do
        form client (Just pk, toForm $ thePayload note) ! [autocomplete_ "off"]


form :: (Monad m) => Stored Client -> (Maybe (Pk Note), Note.Form) -> HtmlT m ()
form client (pk, Note.Form{..}) = do
    form_ ! [ method_ "PUT"
            , action_ $ userUrl client "/note"
            , class_ "note "
            ] $ do
        maybeM_ pk $ \pk -> do
            input_ [type_ "hidden", name_ "id", value_ $ tshow pk]

        div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, placeholder_ "note to self"] $
            maybe "" toHtml text

        maybeM_ pk $ \pk -> do
            select_ ! [name_ "review_status"] $ do
                forM_ [Inbox, Alive, Retired] $ \status -> do
                    option_ ! (if review_status == Just status then [selected_ "true"] else []) $ toHtml (tshow status)

        div_ $ do
            button_ ! [type_ "submit"] $ maybe "Create" (const "Save") pk
            button_ ! [type_ "reset"] $ "Cancel"