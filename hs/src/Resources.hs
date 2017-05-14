{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, LambdaCase #-}
{-#LANGUAGE RankNTypes #-}
module Resources where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Lucid
import Data.Time.Format

import qualified Network.HTTP.Types as Http

import Data.Maybe
import Control.Arrow
import Control.Monad
import qualified Control.Exception as Exn

import Data.Db
import Util
import Html
import Neptune

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import qualified Html.ActionItem as ActionItem


index_R :: Db -> NeptuneApp
index_R db req = do
    when (method req /= "GET") $ Exn.throw (BadVerb ["GET"])
    action_items <- transact db $ ActionItem.all
    render <- throwLeft $ negotiateMedia [("text/plain", text_F), ("text/html", html_F)] (acceptMedia $ negotiation req)
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ action_items) render }
    where
    text_F action_items = LT.encodeUtf8 . LT.pack $ unlines $ show <$> action_items
    html_F action_items = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ do
            ActionItem.form
            div_ ! [id_ "action_items"] $
                forM_ action_items ActionItem.full

action_item_R :: Db -> Maybe (Pk ActionItem) -> NeptuneApp
action_item_R db pk_m req = case method req of
    "GET" -> do
        let pk = fromMaybe (error "404") pk_m
        item <- transact db $ ActionItem.byPk pk >>= \case
            Nothing -> error "404" -- TODO
            Just item -> pure item
        render <- throwLeft $ negotiateMedia [("text/html", html_F), ("text/html+frag", htmlfrag_F)] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ item) render }
    "PUT" -> do
        item <- throwMaybe (error "bad form data" :: Error) fromForm
        item <- transact db $ ActionItem.create item
        render <- throwLeft $ negotiateMedia [("text/html", html_F), ("text/html+frag", htmlfrag_F)] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ item) render }
    _ -> Exn.throw $ BadVerb ["GET", "PUT"]
    where
    html_F item = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ ActionItem.full item
    htmlfrag_F = renderBS . ActionItem.full
    fromForm = do
        let q = snd $ resourceId req
        text <- T.decodeUtf8 <$> query_queryOne q "text" -- FIXME url encoding seems to already happen, but where?
        action_type <- T.decodeUtf8 <$> query_queryOne q "action_type"
        weight <- T.decodeUtf8 <$> query_queryOne q "weight"
        timescale <- T.decodeUtf8 <$> query_queryOne q "timescale"
        action_status <- T.decodeUtf8 <$> query_queryOne q "action_status"
        deadline <- case query_queryOne q "deadline" of
            Nothing -> pure Nothing
            Just (T.unpack . T.decodeUtf8 -> raw) -> do
                day <- parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) raw
                pure $ Just day
        Just ActionItem{..}