module Statistics.GModeling.Gibbs
  (
    Reader (..)
  , Writer (..)
  ) where

data Reader k v = Reader
  {
    -- | Number of available indices
    size :: Int
    -- | Read the value at the given key
  , readn :: Int -> k -> v
    -- | Create a copy for writing only
  , copy :: IO (Writer k v)
  }

data Writer k v = Writer
  {
    -- | Write the value at the given key
    writen :: Int -> k -> v -> IO ()
    -- | Create a read-only copy
  , readOnly :: IO (Reader k v)
  }
