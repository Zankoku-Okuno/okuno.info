{-#LANGUAGE RankNTypes #-}
module Data.RefTables
    ( initialize
    , action_status, weight, timescale
    ) where

import ClassyPrelude

import System.IO.Unsafe
import Data.Db


initialize :: Db  -> IO ()
initialize db = do
    writeIORef _action_status_ref =<< _getDescs [pgSQL|SELECT description FROM rt.action_status ORDER BY ordering ASC;|]
    writeIORef _weight_ref        =<< _getDescs [pgSQL|SELECT description FROM rt.weight        ORDER BY weight ASC;|]
    writeIORef _timescale_ref     =<< _getDescs [pgSQL|SELECT description FROM rt.timescale     ORDER BY time ASC;|]
    where
    _getDescs q = transact db (query q)


action_status, weight, timescale :: [Text]
action_status = unsafePerformIO $ readIORef _action_status_ref
weight = unsafePerformIO $ readIORef _weight_ref
timescale = unsafePerformIO $ readIORef _timescale_ref

_action_status_ref, _weight_ref, _timescale_ref :: IORef [Text]
_action_status_ref = unsafePerformIO $ newIORef (error "Data.refTables._action_status_ref uninitialized")
_weight_ref = unsafePerformIO $ newIORef (error "Data.refTables._weight_ref uninitialized")
_timescale_ref = unsafePerformIO $ newIORef (error "Data.refTables._timescale_ref uninitialized")
