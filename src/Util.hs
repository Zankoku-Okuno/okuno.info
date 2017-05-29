module Util (
      runMaybe
    , throwMaybe
    , throwLeft
    , throwLeftM
    , maybeM_
    , showTime, readTime
    , fileStr
    ) where

import ClassyPrelude
import Language.Haskell.TH

import Data.Time.Format hiding (readTime)
import System.FilePath
import qualified Database.PostgreSQL.Simple as Sql




runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe Nothing x _ = x
runMaybe (Just v) _ f = f v

throwLeft :: (Monad m, MonadThrow m) => Exception e => Either e a -> m a
throwLeft (Left e) = throw e
throwLeft (Right v) = pure v

throwLeftM :: (Monad m, MonadThrow m) => Exception e => Either e (m a) -> m a
throwLeftM (Left e) = throw e
throwLeftM (Right action) = action

throwMaybe :: (Monad m, MonadThrow m) => Exception e => e -> Maybe a -> m a
throwMaybe e Nothing = throw e
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
    bytes <- runIO $ readFile filepath
    let str = unpack . decodeUtf8 $ bytes
    litE $ stringL str