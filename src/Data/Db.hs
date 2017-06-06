{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE RankNTypes #-}
module Data.Db (
      Pk(..)
    , Stored(..)
    , Sql
    , Db, transact
    , query
    , execute
    -- re-exports
    , module Database.PostgreSQL.Typed
) where

import ClassyPrelude
import Text.Read (readsPrec)

import Data.Aeson

import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Query


newtype Pk a = Pk { unPk :: Int32 }
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

newtype Sql a = Sql { unSql :: ReaderT PGConnection IO a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadIO)

type Db = forall a. (PGConnection -> IO a) -> IO a
transact :: Db -> Sql a -> IO a
transact withConnection (Sql action) =
    withConnection $ \conn ->
        pgTransaction conn $ runReaderT action conn

query :: PGQuery q r => q -> Sql [r]
query q = Sql $ do
    conn <- ask
    liftIO $ pgQuery conn q

execute :: PGQuery q () => q -> Sql Int
execute q = Sql $ do
    conn <- ask
    liftIO $ pgExecute conn q
