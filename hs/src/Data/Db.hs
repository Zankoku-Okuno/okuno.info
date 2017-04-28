module Data.Db where

-- monads
import Control.Monad.Reader
-- database
import qualified Database.PostgreSQL.Simple as Sql

newtype Db a = Db { unDb :: Reader Sql.Connection a }