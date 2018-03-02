{-#LANGUAGE MultiParamTypeClasses #-}
module Form.Note where

import ClassyPrelude

import Data.Default
import Data.Db
import Form

import Data.Client (Client)
import Data.Note (Note(..), NoteState(..))
import qualified Data.Note as Note

data Form = Form
    { text :: Maybe Text
    , owner :: Maybe (Pk Client)
    , review_status :: Maybe NoteState
    , review_cycles :: Maybe Int32
    }

instance Default Form where
    def = Form
        { text = Nothing
        , owner = Nothing
        , review_status = Nothing
        , review_cycles = Nothing
        }

instance ToForm Note Form where
    toForm Note{..} = Form
        { text = Just text
        , owner = Just owner
        , review_status = Just review_status
        , review_cycles = Just review_cycles
        }

instance FromForm Note Form where
    fromForm Form{..} = do
        text <- text
        owner <- owner
        review_status <- review_status <|> Just def
        review_cycles <- review_cycles <|> Just 0
        pure Note{..}

instance PatchForm Note Form where
    patchForm note Form{..} = note
        { text = fromMaybe (Note.text note) text
        , owner = fromMaybe (Note.owner note) owner
        , review_status = fromMaybe (Note.review_status note) review_status
        , review_cycles = fromMaybe (Note.review_cycles note) review_cycles
        }
