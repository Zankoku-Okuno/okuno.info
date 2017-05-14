{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Html (
      module Lucid
    , dropdown_, (!)

    , defaultHead
    ) where

import Data.Text (Text)

import Control.Monad

import Lucid
import Lucid.Base (TermRaw(..))


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

    termRawWith "script" [ type_ "text/javascript", src_ "static/better-dom.js"] ""
    termRawWith "script" [ type_ "text/javascript", src_ "static/better-i18n-plugin.js"] ""
    termRawWith "script" [ type_ "text/javascript", src_ "static/better-time-element.js"] ""
    termRawWith "script" [ type_ "text/javascript", src_ "static/better-dateinput-polyfill.js"] ""

    termRawWith "script" [src_ "http://rsvpjs-builds.s3.amazonaws.com/rsvp-latest.js"] ""
    termRawWith "script" [src_ "static/lodash.core.js"] ""
    termRawWith "script" [src_ "static/URI.js"] ""
    termRawWith "script" [src_ "static/http.js"] ""
    
    termRawWith "script" [src_ "static/main.js"] ""