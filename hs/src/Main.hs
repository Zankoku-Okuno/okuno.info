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

import Resources




main :: IO ()
main = do
    config <- loadConfig
    pool <- createPool (Sql.connectPostgreSQL $ connect config) Sql.close 1 10 10 -- TODO configurable timeout/max connections (and whether to pool at all)
    let db = withResource pool
    let port = 8080
    putStrLn $ "Running server on port " ++ show port ++ " (Ctrl-C to exit)..."
    Warp.run port (toWaiApp $ neptune [action_handlers db] haikuErrorResponse)


data Config = Config
    { connect :: BS.ByteString
    }
loadConfig :: IO Config
loadConfig = do
    -- FIXME set location of configs in a command line option
    connect <- BS.readFile "dbconnect.conf"
    pure $ Config {..}


action_handlers :: Db -> Dispatcher
action_handlers db ([], q) = Just $ index_R db
action_handlers db (["action-item"], q) = do
    let pk = Pk . read . T.unpack . T.decodeUtf8 <$> query_queryOne q "id"
    Just $ action_item_R db pk
action_handlers db (["static", filename], q) = Just $ \req -> do
    contents <- LBS.readFile $ "static" </> T.unpack filename
    pure $ Response { status = Http.status200, responseBody = Just ("application/javascript", contents) }
action_handlers db _ = Nothing



