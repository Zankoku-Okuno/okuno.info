{-#LANGUAGE OverloadedStrings #-}
module Util where

import Language.Haskell.TH

import Data.String(IsString(..))
import System.FilePath
import System.IO

import Control.Exception (Exception)
import qualified Control.Exception as Exn

import qualified Database.PostgreSQL.Simple as Sql


runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe Nothing x _ = x
runMaybe (Just v) _ f = f v

throwLeft :: Exception e => Either e a -> IO a
throwLeft (Left e) = Exn.throw e
throwLeft (Right v) = pure v


{-| Create a string literal from the contents of the passed file.

    The file is relative to the module where this is called.
-}
fileStr :: FilePath -> Q Exp
fileStr filename = do
    loc <- loc_filename <$> location
    let filepath = dropFileName loc </> filename
    str <- runIO $ readFile filepath
    litE $ stringL str