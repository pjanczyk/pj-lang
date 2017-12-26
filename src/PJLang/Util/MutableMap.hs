module PJLang.Util.MutableMap (MutableMap, empty, get, put, contains) where

import Data.IORef
import qualified Data.Map.Strict as Map
    
type MutableMap k v = IORef (Map.Map k v)

empty :: IO (MutableMap k v)
empty = newIORef Map.empty

get :: Ord k => MutableMap k v -> k -> IO (Maybe v)
get mm k = Map.lookup k <$> readIORef mm

put :: Ord k => MutableMap k v -> (k, v) -> IO ()
put mm (k, v) = modifyIORef' mm (Map.insert k v)

contains :: Ord k => MutableMap k v -> k -> IO (Bool)
contains mm k = Map.member k <$> readIORef mm