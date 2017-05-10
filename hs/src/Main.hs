{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns, TupleSections, ScopedTypeVariables,
            DeriveAnyClass #-}
module Main where

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as Http
import Network.HTTP.Media
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified Database.PostgreSQL.Simple as Sql
import qualified Data.ActionItem as ActionItem
import Data.Db

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Control.Arrow
import Control.Monad
import Control.Exception (Exception)
import qualified Control.Exception as Exn


runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe Nothing x _ = x
runMaybe (Just v) _ f = f v

throwLeft :: Exception e => Either e a -> IO a
throwLeft (Left e) = Exn.throw e
throwLeft (Right v) = pure v

withConn :: (Sql.Connection -> IO a) -> IO a
withConn transact = Exn.bracket (Sql.connectPostgreSQL "dbname='hstest' port='5432'") Sql.close transact





main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Running server on port " ++ show port ++ " (Ctrl-C to exit)..."
    Warp.run port appServer

appServer :: Wai.Application
appServer waiReq respond = do
    let req@Request{..} = fromWaiReq waiReq
    let pipeline = case dispatch [isIndex] resourceId of
            Nothing -> error "Error 404, that file cannot be found. Try again later." -- FIXME create a Neptune Error
            Just handler -> (Http.status200,) <$> handler req
    response <- Exn.catch pipeline (pure . basicErrorResponse)
    respond $ uncurry toWaiResponse response

fromWaiReq :: Wai.Request -> Request
fromWaiReq req = Request
    { resourceId = (Wai.pathInfo req, fromWaiQuery $ Wai.queryString req)
    , method = Wai.requestMethod req
    , negotiation = Negotiation
        { acceptMedia = fromMaybe (fromJust $ parseQuality "*/*") $
                            parseQuality =<< lookup "Accept" (Wai.requestHeaders req)
        }
    }
fromWaiQuery :: Http.Query -> Query
fromWaiQuery [] = Map.empty
fromWaiQuery ((k, fromMaybe "" -> v) : qs) = Map.insertWith (flip (++)) k [v] (fromWaiQuery qs)

toWaiResponse :: Http.Status -> Response -> Wai.Response
toWaiResponse status Response{ responseBody = Nothing } = Wai.responseLBS status [] ""
toWaiResponse status Response{ responseBody = Just responseBody } = Wai.responseLBS status headers (snd responseBody)
    where
    headers = [("Content-Type", renderHeader (fst responseBody))]

basicErrorResponse :: Error -> (Http.Status, Response)
basicErrorResponse (BadMedia _) = --FIXME include the acceptable media types
    (Http.status406, Response { responseBody = Nothing })


----------------------------------------------

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
    { responseBody :: Maybe Content
    }
data Error
    = BadMedia [MediaType]
    deriving(Show)
instance Exception Error

type NeptuneApp = Request -> IO Response


dispatch :: [Location -> Maybe NeptuneApp] -> Location -> Maybe NeptuneApp
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


----------------------------------------------


isIndex :: Location -> Maybe NeptuneApp
isIndex ([], q) = Just index_R
isIndex _ = Nothing


index_R :: NeptuneApp
index_R req = do
    action_items <- withConn $ transact $ ActionItem.all
    render <- throwLeft $ negotiateMedia [("text/plain", text_F)] (acceptMedia $ negotiation req)
    pure $ Response { responseBody = Just $ second ($ action_items) render }
    where
    text_F action_items = LT.encodeUtf8 . LT.pack $ unlines $ show <$> action_items
    accept = LBS.fromStrict . renderHeader $ acceptMedia (negotiation req)



response404 = Wai.responseLBS Http.status404 [] "Error 404\nthe file could not be found:\ntry again later"
response406 = Wai.responseLBS Http.status406 [] "Error 406\nrequested unknown format,\nnot acceptable"