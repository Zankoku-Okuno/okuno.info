{-#LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Form where

import ClassyPrelude
import Data.Db


class ToForm a form | a -> form where
    toForm :: a -> form

instance ToForm a form => ToForm (Stored a) (Pk a, form) where
    toForm (Stored pk x) = (pk, toForm x)


class FromForm a form | a -> form where
    fromForm :: form -> Maybe a


class PatchForm a form | a -> form where
    patchForm :: a -> form -> a

instance PatchForm a form => PatchForm (Stored a) form where
    patchForm (Stored pk x) form = Stored pk (patchForm x form)