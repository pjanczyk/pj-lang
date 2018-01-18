module PJLang.Util.MutableMap (MutableMap, empty, get, put, contains) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map

-- | A mutable map
newtype MutableMap k v = MutableMap (IORef (Map.Map k v))

-- | Creates an empty map
empty :: IO (MutableMap k v)
empty = MutableMap <$> newIORef Map.empty

-- | Returns a value associated with a given key
get :: Ord k => MutableMap k v -> k -> IO (Maybe v)
get (MutableMap ref) k = Map.lookup k <$> readIORef ref

-- | Changes a value associated with a given key
put :: Ord k => MutableMap k v -> (k, v) -> IO ()
put (MutableMap ref) (k, v) = modifyIORef' ref (Map.insert k v)

-- | Checks whether a map contains a given key
contains :: Ord k => MutableMap k v -> k -> IO (Bool)
contains (MutableMap ref) k = Map.member k <$> readIORef ref