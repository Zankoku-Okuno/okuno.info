module Util (
      module Data.Default
    , module Data.Maybe
    , runMaybe
    , throwMaybe
    , throwLeft
    , maybeM_
    , module Data.Monoid
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Monad
    , getCurrentTime, UTCTime(..), Day
    , showTime, readTime
    , fileStr
    ) where

import Language.Haskell.TH

import Data.Time (getCurrentTime, UTCTime(..), Day)
import Data.Time.Format hiding (readTime)

import System.FilePath
import System.IO

import Data.Default
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Exception (Exception)
import qualified Control.Exception as Exn

import qualified Database.PostgreSQL.Simple as Sql


runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe Nothing x _ = x
runMaybe (Just v) _ f = f v

throwLeft :: Monad m => Exception e => Either e a -> m a
throwLeft (Left e) = Exn.throw e
throwLeft (Right v) = pure v

throwMaybe :: Monad m => Exception e => e -> Maybe a -> m a
throwMaybe e Nothing = Exn.throw e
throwMaybe e (Just x) = pure x


maybeM_ :: Monad m => Maybe a -> (a -> m b) -> m ()
maybeM_ Nothing f = pure ()
maybeM_ (Just x) f = void $ f x

showTime :: FormatTime t => t -> String
showTime = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

readTime :: (Monad m, ParseTime t) => String -> m t
readTime = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing)


{-| Create a string literal from the contents of the passed file.

    The file is relative to the module where this is called.
-}
fileStr :: FilePath -> Q Exp
fileStr filename = do
    loc <- loc_filename <$> location
    let filepath = dropFileName loc </> filename
    str <- runIO $ readFile filepath
    litE $ stringL str