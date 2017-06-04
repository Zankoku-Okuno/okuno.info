{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE RankNTypes #-}
module Data.Db (
      Pk(..)
    , Stored(..)
    , Sql
    , Db, transact
    , query, query_
    , execute, execute_
    -- re-exports
    , Sql.Only(..)
) where

import ClassyPrelude
import Text.Read (readsPrec)

import Data.Aeson

import Database.PostgreSQL.Simple (ToRow, FromRow)
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField


newtype Pk a = Pk Int64
instance Eq (Pk a) where
    (Pk a) == (Pk b) = a == b
instance Show (Pk a) where
    show (Pk a) = show a
instance Read (Pk a) where
    readsPrec n str = first Pk <$> readsPrec n str
instance ToJSON (Pk a) where
    toJSON (Pk x) = toJSON x

data Stored a = Stored { thePk :: Pk a, thePayload :: a }
    deriving (Show)

newtype Sql a = Sql { unSql :: ReaderT Sql.Connection IO a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadIO)

type Db = forall a. (Sql.Connection -> IO a) -> IO a
transact :: Db -> Sql a -> IO a
transact withConnection (Sql action) =
    withConnection $ \conn ->
        Sql.withTransaction conn $ runReaderT action conn

query :: (ToRow q, FromRow r) => Sql.Query -> q -> Sql [r]
query q args = Sql $ do
    conn <- ask
    liftIO $ Sql.query conn q args

query_ :: FromRow r => Sql.Query -> Sql [r]
query_ q = Sql $ do
    conn <- ask
    liftIO $ Sql.query_ conn q

execute :: ToRow q => Sql.Query -> q -> Sql Int64
execute q args = Sql $ do
    conn <- ask
    liftIO $ Sql.execute conn q args

execute_ :: Sql.Query -> Sql Int64
execute_ q = Sql $ do
    conn <- ask
    liftIO $ Sql.execute_ conn q


instance FromField (Pk a) where
    fromField f dat = Pk <$> fromField f dat
instance ToField (Pk a) where
    toField (Pk id) = toField id
