{-#LANGUAGE RecordWildCards #-}
module Data.ActionItem where

-- bytes/text
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import Data.String (IsString(..))

import Data.Time.Clock
import Data.Db


data ActionItem = ActionItem
    { text :: Text
    , created :: UTCTime
    , redline :: Maybe UTCTime
    , deadline :: Maybe UTCTime
    }

create :: ActionItem -> Db Int
create ActionItem{..} = undefined