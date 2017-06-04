{-#LANGUAGE RankNTypes #-}
module Resources where

import ClassyPrelude
import Text.Read (read)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Lucid
import Data.Aeson

import qualified Network.HTTP.Types as Http

import Data.Default
import Util
import Data.Db
import Form
import Html
import Neptune

import Data.ActionItem (ActionItem(..))
import qualified Data.ActionItem as ActionItem
import qualified Html.ActionItem as ActionItem
import qualified Form.ActionItem as ActionItem
import Data.Project (Project(..))
import qualified Data.Project as Project
import qualified Html.Project as Project
import qualified Form.Project as Project
import Data.Client (Client(..))
import qualified Data.Client as Client


index_R :: Db -> NeptuneApp
index_R db req = throwLeftM $ verb (method req) $
    "GET" >: do
        today <- utctDay <$> getCurrentTime
        (projects, action_itemss) <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName "okuno" -- TODO
            projects <- Project.byClient client
            let active_projects = filter (\(Stored _ Project{..}) -> action_status == "active") projects
                action_lists = Nothing : (Just <$> active_projects)
            action_items <- ActionItem.dashboard client `mapM` action_lists
            pure (projects, action_items)
        render <- throwLeft $ negotiateMedia [("text/html", html_F (today, projects))] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ action_itemss) render }
    where
    html_F :: (Day, [Stored Project]) -> [[Stored ActionItem]] -> LBS.ByteString
    html_F more@(today, projects) action_itemss = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ do
            div_ ! [style_ "display: flex; "] $ do
                ActionItem.form projects def
                a_ ! [href_ "/projects"] $ "All Projects"
            hr_ []
            div_ ! [ style_ "display: flex; justify-content: space-around; "] $ do
                forM_ action_itemss $ \action_items -> do
                    div_ $ do
                        ol_ ! [id_ "action_items"] $ -- FIXME id_ is inappropriate
                            forM_ action_items $ \item -> do
                                li_ ! [data_ "action_item" (tshow $ thePk item)] $ ActionItem.full more item

projects_R :: Db -> NeptuneApp
projects_R db req = throwLeftM $ verb (method req) $
    "GET" >: do
        projects <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName "okuno" -- TODO
            Project.byClient client
        render <- throwLeft $ negotiateMedia [("text/html", html_F)] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ projects) render }
    where
    html_F :: [Stored Project] -> LBS.ByteString
    html_F projects = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ do
            Project.form def
            ol_ ! [id_ "projects"] $ forM_ projects $ \project -> do
                li_ $ Project.full project

project_R :: Db -> Maybe (Pk Project) -> NeptuneApp
project_R db pk req = do
    render <- throwLeft $ negotiateMedia [("application/htmlfrag+json", htmlfrag_F)] (acceptMedia $ negotiation req)
    project <- throwLeftM $ verbs (method req)
        [ "GET" >: do
            pk <- throwMaybe BadResource pk
            transact db $ throwMaybe BadResource =<< Project.byPk pk
        , "PUT" >: do
            let form = getForm (snd $ resourceId req) :: Project.Form
            project <- throwMaybe (error "bad form data" :: Error) $ fromForm form -- TODO
            case pk of
                Nothing -> transact db $ do
                    client <- throwMaybe BadResource =<< Client.byName "okuno" -- TODO
                    Project.create client project
                Just pk -> transact db $ do
                    result <- Project.update (Stored pk project)
                    throwMaybe BadResource result
        ]
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ project) render } -- FIXME status201 where appropriate
    where
    htmlfrag_F project =
        let html = renderText $ Project.full project
            json = object ["id" .= thePk project, "project" .= html]
        in encode json
    getForm q = Project.Form
        { name = decodeUtf8 <$> query_queryOne q "name"
        , mission = decodeUtf8 <$> query_queryOne q "mission"
        , action_type = decodeUtf8 <$> query_queryOne q "action_type"
        , action_status = decodeUtf8 <$> query_queryOne q "action_status"
        }

action_item_R :: Db -> Maybe (Pk ActionItem) -> NeptuneApp
action_item_R db pk req = do
    today <- utctDay <$> getCurrentTime
    projects <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName "okuno" -- TODO
            Project.byClient client
    render <- throwLeft $ negotiateMedia [("text/html", html_F (today, projects)), ("application/htmlfrag+json", htmlfrag_F (today, projects))] (acceptMedia $ negotiation req)
    item <- throwLeftM $ verbs (method req)
        [ "GET" >: do
            pk <- throwMaybe BadResource pk
            transact db $ ActionItem.byPk pk >>= \case
                Nothing -> throw BadResource
                Just item -> pure item
        , "PUT" >: do
            let form = getForm (snd $ resourceId req) :: ActionItem.Form
            item <- throwMaybe (error "bad form data" :: Error) $ fromForm form -- TODO
            case pk of
                Nothing -> transact db $ do
                    client <- throwMaybe BadResource =<< Client.byName "okuno" -- TODO
                    ActionItem.create client item
                Just pk -> transact db $ do
                    result <- ActionItem.update (Stored pk item)
                    throwMaybe BadResource result
        ]
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ item) render } -- FIXME status201 where appropriate
    where
    html_F more item = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ ActionItem.full more item
    htmlfrag_F more item = 
        let html = renderText $ ActionItem.full more item
            json = object ["id" .= thePk item, "action_item" .= html]
        in encode json
    getForm q = ActionItem.Form
        { text = decodeUtf8 <$> query_queryOne q "text" -- FIXME url encoding seems to already happen, but where?
        , project = Just $ Pk . read . unpack . decodeUtf8 <$> query_queryOne q "project"
        , action_type = decodeUtf8 <$> query_queryOne q "action_type"
        , action_status = decodeUtf8 <$> query_queryOne q "action_status"
        , weight = decodeUtf8 <$> query_queryOne q "weight"
        , timescale = decodeUtf8 <$> query_queryOne q "timescale"
        , deadline = readTime =<< unpack . decodeUtf8 <$> query_queryOne q "deadline"
        , behalf_of = Just $ decodeUtf8 <$> query_queryOne q "behalf_of"
        }