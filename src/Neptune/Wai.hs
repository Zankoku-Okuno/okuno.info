{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Neptune.Wai where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T

import qualified Network.HTTP.Types as Http
import Network.HTTP.Media
import qualified Network.Wai as Wai

import Data.Maybe
import qualified Data.Map as Map

import Neptune


toWaiApp :: NeptuneApp -> Wai.Application
toWaiApp app waiReq respond = do
    let req = fromWaiReq waiReq
    response <- app req
    respond $ toWaiResponse response


fromWaiQuery :: Http.Query -> Query
fromWaiQuery [] = Map.empty
fromWaiQuery ((k, fromMaybe "" -> v) : qs) = Map.insertWith (flip (++)) k [v] (fromWaiQuery qs)


fromWaiReq :: Wai.Request -> Request
fromWaiReq req = Request
    { resourceId = (Wai.pathInfo req, fromWaiQuery $ Wai.queryString req)
    , method = Wai.requestMethod req
    , negotiation = Negotiation
        { acceptMedia = fromMaybe (fromJust $ parseQuality "*/*") $
                            parseQuality =<< lookup "Accept" (Wai.requestHeaders req)
        }
    }


toWaiResponse :: Response -> Wai.Response
toWaiResponse Response{ responseBody = Nothing, .. } = Wai.responseLBS status [] ""
toWaiResponse Response{ responseBody = Just responseBody, .. } = Wai.responseLBS status headers (snd responseBody)
    where
    headers = [("Content-Type", renderHeader (fst responseBody))]
