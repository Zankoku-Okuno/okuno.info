{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections, ScopedTypeVariables,
            DeriveAnyClass #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TemplateHaskell #-}
module Main where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Lucid
import Lucid.Base (TermRaw(..))

import Data.Time.Calendar (Day)

import Data.Time.Format
import qualified Network.HTTP.Types as Http
import Network.HTTP.Media
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import System.FilePath

import qualified Database.PostgreSQL.Simple as Sql
import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import Data.Db
import Data.Pool

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
    config <- loadConfig
    pool <- createPool (Sql.connectPostgreSQL $ connect config) Sql.close 1 10 10 -- TODO configurable timeout/max connections (and whether to pool at all)
    let db = withResource pool
    let port = 8080
    putStrLn $ "Running server on port " ++ show port ++ " (Ctrl-C to exit)..."
    Warp.run port (toWaiApp $ neptune [action_handlers db] haikuErrorResponse)

loadConfig :: IO Config
loadConfig = do
    -- FIXME set location of configs in a command line option
    connect <- BS.readFile "dbconnect.conf"
    pure $ Config {..}




----------------------------------------------

data Config = Config
    { connect :: BS.ByteString
    }


action_handlers :: Db -> Dispatcher
action_handlers db ([], q) = Just $ index_R db
action_handlers db (["static", filename], q) = Just $ \req -> do
    contents <- LBS.readFile $ "static" </> T.unpack filename
    pure $ Response { status = Http.status200, responseBody = Just ("application/javascript", contents) }
action_handlers db (["delme"], q) = do
    text <- T.decodeUtf8 <$> query_queryOne q "text" -- FIXME url encoding seems to already happen, but where?
    action_type <- T.decodeUtf8 <$> query_queryOne q "action_type"
    weight <- T.decodeUtf8 <$> query_queryOne q "weight"
    timescale <- T.decodeUtf8 <$> query_queryOne q "timescale"
    action_status <- T.decodeUtf8 <$> query_queryOne q "action_status"
    deadline <- case query_queryOne q "deadline" of
        Nothing -> Just Nothing
        Just (T.unpack . T.decodeUtf8 -> raw) -> do
            day <- parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) raw :: Maybe Day
            pure $ Just day
    -- let deadline = (parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . T.unpack . T.decodeUtf8) <$> 
    Just $ delme_R db ActionItem{..}
action_handlers db _ = Nothing


index_R :: Db -> NeptuneApp
index_R db req = do
    action_items <- transact db $ ActionItem.all
    render <- throwLeft $ negotiateMedia [("text/plain", text_F), ("text/html", html_F)] (acceptMedia $ negotiation req)
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ action_items) render }
    where
    text_F action_items = LT.encodeUtf8 . LT.pack $ unlines $ show <$> action_items
    html_F action_items = renderBS $ doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ "残酷 奧泉 ꙮ‽"
            termRawWith "script" [src_ "https://brianblakely.github.io/nodep-date-input-polyfill/nodep-date-input-polyfill.dist.js"] ""
            
            termRawWith "script" [src_ "http://rsvpjs-builds.s3.amazonaws.com/rsvp-latest.js"] ""
            termRawWith "script" [src_ "static/lodash.core.js"] ""
            termRawWith "script" [src_ "static/URI.js"] ""
            termRawWith "script" [src_ "static/http.js"] ""
            
            termRawWith "script" [src_ "static/main.js"] ""
        body_ $ do
            p_ "Goodbyte, cruel world!"
            form_ ! [id_ "create-action-item", spellcheck_ "true"] $ do
                div_ $ textarea_ ! [name_ "text", required_ "true", autofocus_, autocomplete_ "off", placeholder_ "describe action item"] $ mempty
                div_ $ do
                    dropdown_ (Left "select type") ["negentropy", "intel", "decision", "artifact", "learning"] ! [name_ "action_type", required_ "true"]
                    dropdown_ (Left "select timescale") ["hours", "days", "weeks", "months", "years"] ! [name_ "timescale", required_ "true"]
                    dropdown_ (Left "select weight") ["trivial", "minor", "medium", "major"] ! [name_ "weight", required_ "true"]
                    dropdown_ (Right "queued") ["proposed", "queued", "active", "complete", "dismissed"] ! [name_ "action_status", required_ "true"]
                div_ $ input_ [type_ "date", name_ "deadline", placeholder_ "due date"]
                div_ $ do
                    button_ ! [type_ "submit"] $ "Create"
                    button_ ! [type_ "reset"] $ "Cancel"
            forM_ action_items $ \(Stored pk ActionItem{..}) -> do
                div_ $ do
                    p_ $ do
                        toHtml text
                    div_ $ do
                        unmaybeM_ deadline $ \deadline -> do
                            let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) deadline
                            small_ $ toHtml date
                            " "
                        small_ $ toHtml timescale
                        " "
                        small_ $ toHtml weight
                        " "
                        small_ $ toHtml action_status
                        " "
                        small_ $ toHtml action_type


dropdown_ :: Either Text Text -> [Text] -> Html ()
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

delme_R :: Db -> ActionItem -> NeptuneApp
delme_R db item req = do
    ids <- transact db $ ActionItem.create item
    pure $ Response { status = Http.status200, responseBody = Nothing }
