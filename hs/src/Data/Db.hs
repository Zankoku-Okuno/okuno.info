{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Db (
      Pk(..)
    , Stored(..)
    , Db, transact
    , query, query_
    -- re-exports
    , Sql.Only(..)
    , Int64
) where

import Data.Int (Int64)
-- monads
import Control.Monad.Reader
-- database
import Database.PostgreSQL.Simple (ToRow, FromRow)
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField


data Pk a = Pk Int64
    deriving (Show)
data Stored a = Stored (Pk a) a
    deriving (Show)


newtype Db a = Db { unDb :: ReaderT Sql.Connection IO a }
    deriving (Functor, Applicative, Monad, MonadIO)
transact :: Db a -> Sql.Connection -> IO a
transact (Db action) conn = Sql.withTransaction conn $ runReaderT action conn

query :: (ToRow q, FromRow r) => Sql.Query -> q -> Db [r]
query q args = Db $ do
    conn <- ask
    liftIO $ Sql.query conn q args

query_ :: FromRow r => Sql.Query -> Db [r]
query_ q = Db $ do
    conn <- ask
    liftIO $ Sql.query_ conn q


instance FromField (Pk a) where
    fromField f dat = Pk <$> fromField f dat
instance ToField (Pk a) where
    toField (Pk id) = toField id
