{-#LANGUAGE OverloadedStrings, RecordWildCards, DuplicateRecordFields, OverloadedLabels, ViewPatterns, LambdaCase #-}
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

import qualified Network.HTTP.Types as Http

import Data.Maybe
import Control.Arrow
import Control.Monad
import qualified Control.Exception as Exn

import Util
import Data.Db
import Form
import Html
import Neptune

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import qualified Html.ActionItem as ActionItem
import qualified Form.ActionItem as ActionItem


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
            ActionItem.form def
            ol_ ! [id_ "action_items"] $
                forM_ action_items $ \item -> do
                    li_ $ (ActionItem.form . first Just . toForm) (item :: Stored ActionItem)

action_item_R :: Db -> Maybe (Pk ActionItem) -> NeptuneApp
action_item_R db pk req = do
    render <- throwLeft $ negotiateMedia [("text/html", html_F), ("text/html+frag", htmlfrag_F)] (acceptMedia $ negotiation req)
    item <- case method req of
        "GET" -> do
            pk <- throwMaybe BadResource pk
            transact db $ ActionItem.byPk pk >>= \case
                Nothing -> Exn.throw BadResource
                Just item -> pure item
        "PUT" -> do
            let form = getForm (snd $ resourceId req) :: ActionItem.Form
            item <- throwMaybe (error "bad form data" :: Error) $ fromForm form -- TODO
            case pk of
                Nothing -> transact db $ ActionItem.create item
                Just pk -> transact db $ do
                    result <- ActionItem.update (Stored pk item)
                    throwMaybe BadResource result
        _ -> Exn.throw $ BadVerb ["GET", "PUT"]
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ item) render }
    where
    html_F item = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ ActionItem.full item
    htmlfrag_F = renderBS . li_ . ActionItem.form . first Just . toForm
    getForm q = ActionItem.Form
        { text = T.decodeUtf8 <$> query_queryOne q "text" -- FIXME url encoding seems to already happen, but where?
        , action_type = T.decodeUtf8 <$> query_queryOne q "action_type"
        , weight = T.decodeUtf8 <$> query_queryOne q "weight"
        , timescale = T.decodeUtf8 <$> query_queryOne q "timescale"
        , action_status = T.decodeUtf8 <$> query_queryOne q "action_status"
        , deadline = readTime =<< T.unpack . T.decodeUtf8 <$> query_queryOne q "deadline"
        }