{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE TemplateHaskell #-}
module Main where

import ClassyPrelude
import Text.Read (read)

import Data.Default
import Util

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Lucid
import Lucid.Base (TermRaw(..))

import Database.PostgreSQL.Typed
import Network (PortID(UnixSocket))
import Data.Db
import Data.Pool

import qualified Network.HTTP.Types as Http
import Network.HTTP.Media
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import System.Environment (getEnv)
import System.FilePath
import System.IO (openFile, IOMode(..))

import Neptune
import Neptune.Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai

import Resources
import Data.RefTables (initialize)
import Data.Client (Username(..))




main :: IO ()
main = do
    Config{..} <- loadConfig
    pool <- createPool (pgConnect dbconnect) pgDisconnect 1 10 10 -- TODO configurable timeout/max connections (and whether to pool at all)
    let db = withResource pool
        app = toWaiApp $ neptune [static_handler, user_handlers db] haikuErrorResponse
    putStrLn $ "Logging to " ++ tshow (fst log)
    initialize db
    putStrLn $ "Running server on port " ++ tshow port ++ " (Ctrl-C to exit)..."
    Warp.run port $ (snd log) app


data Config = Config
    { port :: Int
    , dbconnect :: PGDatabase
    , log :: (FilePath, Wai.Middleware)
    }
loadConfig :: IO Config
loadConfig = do
    -- FIXME real command line option parsing
    config_dir <- getArgs >>= \case
        [filepath] -> pure $ unpack filepath
        _ -> error "usage: okuno-info <config-dir>"
    port <- read . unpack . decodeUtf8 <$> readFile (config_dir </> "port.conf")
    user <- fromString <$> getEnv "USER"
    dbname <- fromString <$> getEnv "TPG_DB"
    dbsock <- UnixSocket <$> getEnv "TPG_SOCK"
    let dbconnect = parseDb user dbname dbsock
    log <- parseLog =<< unpack . decodeUtf8 <$> readFile (config_dir </> "logging.conf")
    pure $ Config {..}
    where
    parseDb user dbname socket = defaultPGDatabase { pgDBName = dbname, pgDBUser = user, pgDBPort = socket }
    parseLog "" = pure $ ("<stdout>", Wai.logStdoutDev)
    parseLog filepath = do
        handle <- openFile filepath AppendMode
        logger <- Wai.mkRequestLogger def{ Wai.outputFormat = Wai.Detailed False
                                         , Wai.destination = Wai.Handle handle
                                         }
        pure (filepath, logger)


user_handlers :: Db -> Route
user_handlers db ((uncons -> Just ('~', Username -> username)) : r, q) = dispatch [action_handlers] (r, q)
    where
    action_handlers ([], q) = Just $ dashboard_R db username
    action_handlers (["action-item"], q) = do
        let pk = Pk . read . unpack . decodeUtf8 <$> query_queryOne q "id"
        Just $ action_item_R db (username, pk)
    action_handlers (["notes"], q) = Just $ notes_R db username
    action_handlers (["project"], q) = do
        let pk = Pk . read . unpack . decodeUtf8 <$> query_queryOne q "id"
        Just $ project_R db (pk, username)
    action_handlers (["projects"], q) = Just $ projects_R db username
    action_handlers (["tags"], q) = Just $ tags_R db username
    action_handlers (["tag"], q) = do
        let pk = Pk . read . unpack . decodeUtf8 <$> query_queryOne q "id"
        Just $ tag_R db (pk, username)
    action_handlers _ = Nothing
user_handlers _ _ = Nothing


static_handler :: Route
static_handler (["static", filename], q) = Just $ \req -> do
    contents <- LBS.readFile $ "static" </> unpack filename
    pure $ Response { status = Http.status200, responseBody = Just (mime filename, contents) }
    where
    mime :: Text -> MediaType
    mime filename
        | ".js" `isSuffixOf` filename  = "application/javascript"
        | ".css" `isSuffixOf` filename  = "text/css"
        | ".html" `isSuffixOf` filename  = "text/html"
        | otherwise = "application/octet-stream"
static_handler _ = Nothing

