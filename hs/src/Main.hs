{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections, ScopedTypeVariables, LambdaCase #-}
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

import System.Environment
import System.FilePath
import System.IO

import qualified Database.PostgreSQL.Simple as Sql
import Data.Db
import Data.Pool

import Data.Default
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
import qualified Network.Wai.Middleware.RequestLogger as Wai

import Resources




main :: IO ()
main = do
    Config{..} <- loadConfig
    pool <- createPool (Sql.connectPostgreSQL dbconnect) Sql.close 1 10 10 -- TODO configurable timeout/max connections (and whether to pool at all)
    let db = withResource pool
        app = toWaiApp $ neptune [action_handlers db] haikuErrorResponse
    putStrLn $ "Logging to " ++ show (fst log)
    putStrLn $ "Running server on port " ++ show port ++ " (Ctrl-C to exit)..."
    Warp.run port $ (snd log) app


data Config = Config
    { port :: Int
    , dbconnect :: BS.ByteString
    , log :: (String, Wai.Middleware)
    }
loadConfig :: IO Config
loadConfig = do
    -- FIXME real command line option parsing
    config_dir <- getArgs >>= \case
        [] -> pure "."
        [filepath] -> pure filepath
        _ -> error "usage: okuno-info [config-dir]"
    port <- read <$> readFile (config_dir </> "port.conf")
    dbconnect <- BS.readFile (config_dir </> "dbconnect.conf")
    log <- parseLog =<< readFile (config_dir </> "logging.conf")
    pure $ Config {..}
    where
    parseLog "" = pure $ ("<stdout>", Wai.logStdoutDev)
    parseLog filepath = do
        handle <- openFile filepath AppendMode
        logger <- Wai.mkRequestLogger def{ Wai.outputFormat = Wai.Detailed False
                                         , Wai.destination = Wai.Handle handle
                                         }
        pure (filepath, logger)


action_handlers :: Db -> Dispatcher
action_handlers db ([], q) = Just $ index_R db
action_handlers db (["action-item"], q) = do
    let pk = Pk . read . T.unpack . T.decodeUtf8 <$> query_queryOne q "id"
    Just $ action_item_R db pk
action_handlers db (["static", filename], q) = Just $ \req -> do
    contents <- LBS.readFile $ "static" </> T.unpack filename
    pure $ Response { status = Http.status200, responseBody = Just ("application/javascript", contents) }
action_handlers db _ = Nothing



