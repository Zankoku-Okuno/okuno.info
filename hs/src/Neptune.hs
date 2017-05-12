{-#LANGUAGE OverloadedStrings, TupleSections, RecordWildCards #-}
module Neptune where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T

import qualified Network.HTTP.Types as Http
import Network.HTTP.Media

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Exception (Exception)
import qualified Control.Exception as Exn


-- resource identifier (not necessarily universal)
type Location = ([Text], Query)
type Query = Map BS.ByteString [BS.ByteString] -- FIXME? convert to text -- FIXME? use lazy bytestrings


query_queryAll :: Query -> BS.ByteString -> [BS.ByteString]
query_queryAll q k = fromMaybe [] $ Map.lookup k q
query_queryOne :: Query -> BS.ByteString -> Maybe BS.ByteString
query_queryOne q k = listToMaybe $ query_queryAll q k

data Request = Request
    { resourceId :: Location
    , method :: Http.Method
    , negotiation :: Negotiation
    }
data Negotiation = Negotiation
    { acceptMedia :: [Quality MediaType]
    -- TODO accept-language
    -- TODO accept-encoding
    }

type Content = (MediaType, LBS.ByteString)
data Response = Response
    { status :: Http.Status
    , responseBody :: Maybe Content
    }
data Error
    = BadResource Location
    | BadMedia [MediaType]
    deriving(Show)
instance Exception Error

type NeptuneApp = Request -> IO Response

type Dispatcher = Location -> Maybe NeptuneApp
type ErrorReporter = Request -> Error -> Response

neptune :: [Dispatcher] -> ErrorReporter -> NeptuneApp
neptune handlers onError req@Request{..} = do
    let pipeline = case dispatch handlers resourceId of
            Nothing -> Exn.throw $ BadResource resourceId
            Just handler -> handler req
    Exn.catch pipeline (pure . onError req)

dispatch :: [Dispatcher] -> Location -> Maybe NeptuneApp
dispatch rs rid = listToMaybe . catMaybes $ ($ rid) <$> rs


negotiate :: Accept a => [(a, b)] -> [Quality a] -> Either [a] (a, b)
negotiate provide accept =
    let provide' = ( \(f, s) -> (f, (f, s)) ) <$> provide -- TODO send in an Issue to http-media
        acceptable = map fst provide
    in case mapQuality provide' accept of
        Nothing -> Left acceptable
        Just result -> Right result
--FIXME I think it'd be more fruitful to allow any `Accept` to have this sort of interface
negotiateMedia :: [(MediaType, b)] -> [Quality MediaType] -> Either Error (MediaType, b)
negotiateMedia provide accept = case negotiate provide accept of
    Left acceptable -> Left $ BadMedia acceptable
    Right result -> Right result


haikuErrorResponse :: Request -> Error -> Response
haikuErrorResponse req (BadResource loc) =
    Response { status = Http.status404, responseBody = Just ("text/plain", "Error 404:\nYour file could not be found,\nTry again later.") }
haikuErrorResponse req (BadMedia _) = --FIXME include the acceptable media types
    Response { status = Http.status406, responseBody = Just ("text/plain", "Error 406:\nUnknown format requested,\nNot acceptable.") }