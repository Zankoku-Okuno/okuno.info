{-#LANGUAGE OverloadedStrings #-}
module Main where

-- bytes/text
import qualified Data.ByteString as BS
import Data.String (IsString(..))

-- control structures
import Control.Exception (Exception)
import qualified Control.Exception as Exn

-- file system
import System.IO
import System.FilePath
-- database
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple (Only(..))
-- models
import Data.ActionItem

main :: IO ()
main = do
    connstr <- BS.readFile $ "secrets" </> "dbconn"
    let withConn = Exn.bracket (Sql.connectPostgreSQL connstr) Sql.close
    let files = ("sql" </>) <$> ["001-action_item.sql"]
    sql_scripts <- mapM readFile files
    withConn $ \conn -> do
        Sql.withTransaction conn $ do
            [Only version] <- Sql.query_ conn "SELECT version FROM version;"
            let scripts = fromString <$> drop version sql_scripts
            mapM_ (Sql.execute_ conn) scripts
    putStrLn "Goodbyte, cruel world!"