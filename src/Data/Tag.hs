module Data.Tag
    ( Tag(..)
    , byId
    , byClient
    , create, update
    ) where

import ClassyPrelude

import Data.Db
import Data.Client (Client)


data Tag = Tag
    { name :: Text
    }
    deriving (Show)


byId :: Pk Tag -> Sql (Maybe (Stored Tag))
byId pk = xform <$> query [pgSQL|
    SELECT id, name
    FROM tag
    WHERE id = ${unPk pk};|]
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

byClient :: Stored Client -> Sql [Stored Tag]
byClient client = (xformRow <$>) <$> query [pgSQL|
    SELECT 
        tag.id,
        name
    FROM tag
    WHERE owner = ${unPk $ thePk client}
    ORDER BY name ASC;|]


create :: Stored Client -> Tag -> Sql (Stored Tag)
create client tag@Tag{..} = do
    ids <- query [pgSQL|
        INSERT INTO tag (owner, name)
        VALUES (${unPk $ thePk client}, ${name})
        RETURNING id;|]
    tag <- case ids of
        [pk] -> pure $ Stored (Pk pk) tag
        _ -> error "sql insert failed"
    pure tag

update :: Stored Tag -> Sql (Maybe (Stored Tag))
update tag@(Stored pk Tag{..}) = do
    ids <- query [pgSQL|
        UPDATE tag SET name = ${name}
        WHERE tag.id = ${unPk pk}
        RETURNING id;|]
    pure $ case ids of
        [] -> Nothing
        [pk :: Int32] -> Just tag
        _ -> error "sql insert failed"

xformRow (id, name) = Stored (Pk id) Tag{..}