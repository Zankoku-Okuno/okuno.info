module Html (
      module Lucid
    , dropdown_, (!)

    , defaultHead
    ) where

import ClassyPrelude

import Lucid
import Lucid.Base (TermRaw(..))


css_ :: Monad m => Text -> HtmlT m ()
css_ href = link_ [rel_ "stylesheet", type_ "text/css", href_ href]
js_ :: Monad m => Text -> HtmlT m ()
js_ href = termRawWith "script" [ type_ "text/javascript", src_ href] ""

dropdown_ :: Monad m => Either Text Text -> [Text] -> HtmlT m ()
dropdown_ sel opts =
    select_ $ do
        emptyOpt
        forM_ opts $ \opt ->
            option_ ! attrs opt $ toHtml opt
    where
    emptyOpt = case sel of
        Left nada -> option_ ! [value_ "", selected_ "true"] $ toHtml nada
        _ -> mempty
    attrs opt = case sel of
        Right sel | sel == opt -> [value_ opt, selected_ "true"]
        _ -> [value_ opt]

(!) :: With a => a -> [Attribute] -> a
(!) = with



defaultHead :: Monad m => HtmlT m () 
defaultHead = head_ $ do
    meta_ [charset_ "utf-8"]
    title_ "残酷 奧泉 ꙮ‽"

    js_ "/static/classList.js"
    js_ "/static/autoresize-textarea.js"

    js_ "/static/better-dom.js"
    js_ "/static/better-i18n-plugin.js"
    js_ "/static/better-time-element.js"
    js_ "/static/better-dateinput-polyfill.js"

    js_ "/static/rsvp-latest.min.js"
    js_ "/static/lodash.core.js"
    js_ "/static/URI.js"
    js_ "/static/http.js"

    js_ "/static/markdown.min.js"
    
    js_ "/static/main.js"
    css_ "/static/main.css"