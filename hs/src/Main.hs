{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections, ScopedTypeVariables,
            DeriveAnyClass #-}
module Main where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as Http
import Network.HTTP.Media
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified Database.PostgreSQL.Simple as Sql
import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import Data.Db

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Exception (Exception)
import qualified Control.Exception as Exn


import Util
import Neptune
import Neptune.Wai




main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Running server on port " ++ show port ++ " (Ctrl-C to exit)..."
    Warp.run port (toWaiApp $ neptune [action_handlers] haikuErrorResponse)





----------------------------------------------


action_handlers :: Location -> Maybe NeptuneApp
action_handlers ([], q) = Just index_R
action_handlers (["delme"], q) = do
    text <- parse <$> query_queryOne q "text"
    Just $ delme_R text
    where
    parse = T.decodeUtf8 -- FIXME url encoding?
action_handlers _ = Nothing


index_R :: NeptuneApp
index_R req = do
    action_items <- withConn $ transact $ ActionItem.all
    render <- throwLeft $ negotiateMedia [("text/plain", text_F)] (acceptMedia $ negotiation req)
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ action_items) render }
    where
    text_F action_items = LT.encodeUtf8 . LT.pack $ unlines $ show <$> action_items
    accept = LBS.fromStrict . renderHeader $ acceptMedia (negotiation req)

delme_R :: Text -> NeptuneApp
delme_R text req = do
    ids <- withConn $ transact $ ActionItem.create ActionItem
        { text = text
        , action_type = "intel"
        , weight = "minor"
        , timescale = "days"
        , deadline = Nothing
        , action_status = "proposed"
        }
    pure $ Response { status = Http.status200, responseBody = Nothing }
