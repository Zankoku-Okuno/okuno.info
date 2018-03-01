{-#LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-#LANGUAGE StandaloneDeriving #-}
module Data.Note
    ( Note(..)
    , NoteState(..)
    , dashboard
    , byPk
    , create
    , update
    ) where

import ClassyPrelude
import Data.Db
import Database.PostgreSQL.Typed.Enum

import Data.Client (Client, Username)


$(dataPGEnum "NoteState" "note_state" id)
deriving instance Show NoteState

data Note = Note
    { text :: Text
    , owner :: Pk Client
    , review_cycles :: Int32
    , review_status :: NoteState
    } deriving (Show)

dashboard :: Stored Client -> Sql [Stored Note]
dashboard client = (xformRow <$>) <$> query [pgSQL|
    SELECT
        id,
        text,
        owner_id,
        review_cycles,
        review_status
    FROM note
    WHERE
        owner_id = ${unPk $ thePk client} AND
        review_status != 'Retired'
    ORDER BY
        review_status = 'Inbox' DESC,
        review_cycles ASC,
        random() DESC;
|]

byPk :: (Stored Client, Pk Note) -> Sql (Maybe (Stored Note))
byPk (client, note_id) =
    xform <$> query [pgSQL|
        SELECT
            id,
            text,
            owner_id,
            review_cycles,
            review_status
        FROM note
        WHERE
            id = ${unPk note_id} AND
            owner_id = ${unPk $ thePk client}
    |]
    where
    xform [] = Nothing
    xform [it] = Just $ xformRow it

create :: (Stored Client, Note) -> Sql (Stored Note)
create (client, note@Note{..}) = do
    ids <- query [pgSQL|
        INSERT INTO note (
            text,
            owner_id,
            review_cycles,
            review_status
        ) VALUES (
            ${text},
            ${unPk owner},
            ${review_cycles},
            ${review_status}
        )
        RETURNING id;
    |]
    pure $ case ids of
        [pk] -> Stored (Pk pk) note
        _ -> error "sql insert failed"

update :: (Stored Client, Pk Note) -> Note -> Sql (Maybe (Stored Note))
update (client, pk) item@Note{..} = do
    ids <- query [pgSQL|
        UPDATE note SET
            text = ${text},
            owner_id = ${unPk owner},
            review_cycles = ${review_cycles},
            review_status = ${review_status}
        WHERE
            id = ${unPk pk} AND
            owner_id = ${unPk $ thePk client}
        RETURNING id;|]
    pure $ case ids of
        [] -> Nothing
        [pk] -> Just $ Stored (Pk pk) item
        _ -> error "sql insert failed"

xformRow :: (Int32, Text, Int32, Int32, NoteState) -> Stored Note
xformRow (id, text, Pk -> owner, review_cycles, review_status) =
    Stored (Pk id) Note {..}