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
import Data.Tag (Tag(..))
import qualified Data.Tag as Tag
import qualified Html.Tag as Tag
import qualified Form.Tag as Tag
import Data.Client (Client(..), Username(..))
import qualified Data.Client as Client
import qualified Html.Client as Client
import Html.Client (userUrl)


dashboard_R :: Db -> Username -> NeptuneApp
dashboard_R db username req = throwLeftM $ verb (method req) $
    "GET" >: do
        today <- utctDay <$> getCurrentTime
        (client, projects, action_itemss) <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName username
            projects <- Project.byClient client
            let active_projects = filter (\(Stored _ Project{..}) -> action_status == "active") projects
                action_lists = Nothing : (Just <$> active_projects)
            action_items <- ActionItem.dashboard client `mapM` action_lists
            pure (client, projects, action_items)
        render <- throwLeft $ negotiateMedia [("text/html", html_F (today, client, projects))] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ action_itemss) render }
    where
    html_F :: (Day, Stored Client, [Stored Project]) -> [[Stored ActionItem]] -> LBS.ByteString
    html_F more@(_, client, projects) action_itemss = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ do
            Client.navigation client
            -- div_ ! [style_ "display: flex; "] $ do
            ActionItem.form (client, projects) def
                -- a_ ! [href_ $ userUrl client "/projects"] $ "All Projects"
            hr_ []
            div_ ! [ style_ "display: flex; justify-content: space-around; "] $ do
                forM_ action_itemss $ \action_items -> do
                    div_ $ do
                        let projname = Nothing -- TODO have the project for the action_items available to render stuff
                        ol_ ! [class_ "action_items ", data_ "project" (fromMaybe "" projname)] $ -- FIXME id_ is inappropriate
                            forM_ action_items $ \item -> do
                                li_ ! [ class_ "action_item "
                                      , data_ "pk" (tshow $ thePk item)
                                      ] $ ActionItem.full more item





projects_R :: Db -> Username -> NeptuneApp
projects_R db username req = throwLeftM $ verb (method req) $
    "GET" >: do
        (client, projects) <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName username
            projects <- Project.byClient client
            pure (client, projects)
        render <- throwLeft $ negotiateMedia [("text/html", html_F client)] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ projects) render }
    where
    html_F :: Stored Client -> [Stored Project] -> LBS.ByteString
    html_F client projects = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ do
            Client.navigation client
            Project.form client def
            ol_ ! [class_ "projects "] $ forM_ projects $ \project -> do
                li_ ! [ class_ "project "
                      , data_ "pk" (tshow $ thePk project)
                      ] $ Project.full client project

project_R :: Db -> (Maybe (Pk Project), Username) -> NeptuneApp
project_R db (pk, username) req = do
    (client, project) <- throwLeftM $ verbs (method req)
        [ "GET" >: do
            pk <- throwMaybe BadResource pk
            transact db $ do
                client <- throwMaybe BadResource =<< Client.byName username
                project <- throwMaybe BadResource =<< Project.byPk pk
                pure (client, project)
        , "PUT" >: do
            transact db $ do
                client <- throwMaybe BadResource =<< Client.byName username
                let form = getForm client (snd $ resourceId req) :: Project.Form
                project <- throwMaybe (error "bad form data" :: Error) $ fromForm form -- TODO
                case pk of
                    Nothing -> do
                        project <- Project.create client project
                        pure (client, project)
                    Just pk -> do
                        project <- throwMaybe BadResource =<< Project.update (Stored pk project)
                        pure (client, project)
        ]
    render <- throwLeft $ negotiateMedia [("application/htmlfrag+json", htmlfrag_F client)] (acceptMedia $ negotiation req)
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ project) render } -- FIXME status201 where appropriate
    where
    htmlfrag_F client project =
        let html = renderText $ Project.full client project
            json = object ["id" .= thePk project, "htmlfrag" .= html]
        in encode json
    getForm client q = Project.Form
        { name = decodeUtf8 <$> query_queryOne q "name"
        , mission = decodeUtf8 <$> query_queryOne q "mission"
        , action_status = decodeUtf8 <$> query_queryOne q "action_status"
        }





tags_R :: Db -> Username -> NeptuneApp
tags_R db username req = throwLeftM $ verb (method req) $
    "GET" >: do
        (client, tags) <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName username
            tags <- Tag.byClient client
            pure (client, tags)
        render <- throwLeft $ negotiateMedia [("text/html", html_F client)] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ tags) render }
    where
    html_F :: Stored Client -> [Stored Tag] -> LBS.ByteString
    html_F client tags = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ do
            Client.navigation client
            Tag.form client def
            ol_ ! [class_ "tags "] $ forM_ tags $ \tag -> do
                li_ ! [ class_ "tag "
                      , data_ "pk" (tshow $ thePk tag)
                      ] $ Tag.full client tag

tag_R :: Db -> (Maybe (Pk Tag), Username) -> NeptuneApp
tag_R db (pk, username) req = do
    (client, tag) <- throwLeftM $ verbs (method req)
        [ "PUT" >: do
            transact db $ do
                client <- throwMaybe BadResource =<< Client.byName username
                let form = getForm client (snd $ resourceId req) :: Tag.Form
                tag <- throwMaybe (error "bad form data" :: Error) $ fromForm form -- TODO
                case pk of
                    Nothing -> do
                        tag <- Tag.create client tag
                        pure (client, tag)
                    Just pk -> do
                        tag <- throwMaybe BadResource =<< Tag.update (Stored pk tag)
                        pure (client, tag)
        ]
    render <- throwLeft $ negotiateMedia [("application/htmlfrag+json", htmlfrag_F client)] (acceptMedia $ negotiation req)
    pure $ Response { status = Http.status200, responseBody = Just $ second ($ tag) render } -- FIXME status201 where appropriate
    where
    htmlfrag_F client tag =
        let html = renderText $ Tag.full client tag
            json = object ["id" .= thePk tag, "htmlfrag" .= html]
        in encode json
    getForm client q = Tag.Form
        { name = decodeUtf8 <$> query_queryOne q "name"
        }





action_item_R :: Db -> (Username, Maybe (Pk ActionItem)) -> NeptuneApp
action_item_R db (username, pk) req = throwLeftM $ verbs (method req) 
    [ "PUT" >: do
        today <- utctDay <$> getCurrentTime
        (client, item, projects) <- transact db $ do
            client <- throwMaybe BadResource =<< Client.byName username
            let form = getForm client (snd $ resourceId req)
            item <- throwMaybe (error "bad form data" :: Error) $ fromForm form
            item <- case pk of
                Nothing -> ActionItem.create (client, item)
                Just pk -> throwMaybe BadResource =<< ActionItem.update (client, pk) item
            projects <- Project.byClient client
            pure (client, item, projects)
        render <- throwLeft $ negotiateMedia
                    [ ("text/html", html_F (today, client, projects))
                    , ("application/htmlfrag+json", htmlfrag_F (today, client, projects))
                    ] (acceptMedia $ negotiation req)
        pure $ Response { status = Http.status200, responseBody = Just $ second ($ item) render } -- FIXME status201 where appropriate
    ]
    where
    html_F more item = renderBS $ doctypehtml_ $ do
        defaultHead
        body_ $ ActionItem.full more item
    htmlfrag_F more item = 
        let html = renderText $ ActionItem.full more item
            json = object ["id" .= thePk item, "htmlfrag" .= html]
        in encode json
    getForm client q = ActionItem.Form
        { text = decodeUtf8 <$> query_queryOne q "text" -- FIXME url encoding seems to already happen, but where?
        , action_status = decodeUtf8 <$> query_queryOne q "action_status"
        , weight = decodeUtf8 <$> query_queryOne q "weight"
        , timescale = decodeUtf8 <$> query_queryOne q "timescale"
        , deadline = readTime =<< unpack . decodeUtf8 <$> query_queryOne q "deadline"
        , project_id = parseProjectId <$> query_queryOne q "project"
        }
        where
        parseProjectId "" = Nothing -- TODO check this actually is what the browser does
        parseProjectId str = Just . Pk . read . unpack . decodeUtf8 $ str
