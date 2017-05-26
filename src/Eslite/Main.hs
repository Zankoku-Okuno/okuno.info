{-#LANGUAGE TypeFamilies #-}
module Main where

import Data.UUID
import Data.UUID.V4 (nextRandom)
import Data.Time.Clock
import Data.Map (Map)
import qualified Data.Map as Map

import Data.IORef


main = putStrLn "foo"



data Event = Event
    { entryUuid :: UUID
    , entryTimestamp :: UTCTime
    , entryData :: Data
    }
data Data = I Integer

class Backend db where
    data CreateOptions db :: *
    data ConnectOptions db :: *
    data Log db :: *
    data View db :: *
    
    createEslite :: CreateOptions db -> IO db
    connectEslite :: ConnectOptions db -> IO db

    createLog :: db -> String -> IO (Log db)
    writeEvent :: Log db -> Data -> IO Event
    
    view :: Log db -> View db
    size :: View db -> IO Int -- TODO uint64
    window :: View db -> (Int, Int) -> IO [Event] -- TODO uint64




data Memory = Mem { fromMem :: IORef (Map String (Log Memory)) }
instance Backend Memory where
    data CreateOptions Memory = CreateMem
    data ConnectOptions Memory = ConnectMem
    newtype Log Memory = LogMem { fromMemLog :: IORef [Event] } -- FIXME use Sequence
    data View Memory
        = ViewLog (Log Memory)

    createEslite _ = do
        cell <- newIORef Map.empty
        pure $ Mem cell
    connectEslite _ = createEslite undefined

    createLog db name = do
        freshLog <- LogMem <$> newIORef []
        modifyIORef (fromMem db) (Map.insertWith (\_ _ -> error $ "log already exists: " ++ show name) name freshLog)
        pure freshLog
    writeEvent log x = do
        uuid <- nextRandom
        now <- getCurrentTime
        let event = Event
                { entryUuid = uuid
                , entryTimestamp = now
                , entryData = x
                }
        modifyIORef (fromMemLog log) (`snoc` event)
        pure event
    
    view = ViewLog
    size (ViewLog log) = length <$> readIORef (fromMemLog log)
    window (ViewLog log) (start, end) = take (end - start) . drop start <$> readIORef (fromMemLog log)


snoc xs x = xs ++ [x]
