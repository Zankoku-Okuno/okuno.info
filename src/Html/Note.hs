module Html.Note where

import ClassyPrelude

import Data.Db
import Form
import Html

import Data.Note (Note(..))
import qualified Data.Note as Note
import Data.Client (Client(..), Username(..))


full :: (Monad m) => Stored Note -> HtmlT m ()
full note@(Stored pk Note{..}) = do
    let tabset = concat ["note_", tshow pk]
    select_ [data_ "tabs" tabset] $ do
        option_ ! [value_ "view", selected_ "true"] $ "View"
        -- option_ ! [value_ "edit"] $ "Edit"
    div_ ! [ data_ "tabset" tabset
           , data_ "tab" "view"
           ] $ do
        div_ ! [class_ "markdown "] $ toHtml text
        div_ ! [class_ "meta "] $ do
            span_ ! [data_ "status" (pack $ show review_status)] $ toHtml (show review_status)
            " "