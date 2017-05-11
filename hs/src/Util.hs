{-#LANGUAGE OverloadedStrings #-}
module Util where

import Control.Exception (Exception)
import qualified Control.Exception as Exn

import qualified Database.PostgreSQL.Simple as Sql


runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe Nothing x _ = x
runMaybe (Just v) _ f = f v

throwLeft :: Exception e => Either e a -> IO a
throwLeft (Left e) = Exn.throw e
throwLeft (Right v) = pure v

withConn :: (Sql.Connection -> IO a) -> IO a
withConn transact = Exn.bracket (Sql.connectPostgreSQL "dbname='hstest' port='5432'") Sql.close transact