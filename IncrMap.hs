module IncrMap (IncrMap, empty, add, updateWith, get) where

import Data.Map (Map)
import qualified Data.Map as Map

type IncrMap a = (Int, Map Int a)

-- | An empty IncrMap. The MID is initialized to 1.
empty :: IncrMap a
empty = (1, Map.empty)

-- | Add the argued value to the argued IncrMap. Return the ID of
-- | the added value along with the updated IncrMap.
add :: IncrMap a -> a -> (Int, IncrMap a)
add (k, m) c = (k, (k + 1, Map.insert k c m))

-- | Update an existing value with the argued MID in the argued 
-- | IncrMap by applying an argued function to it.
updateWith :: Int -> (a -> a) -> IncrMap a -> Maybe (IncrMap a)
updateWith mid f (k, m) =
    case Map.lookup mid m of
        Just c -> Just (k, Map.insert mid (f c) m)
        _      -> Nothing

-- | Return the value from the argued IncrMap with the argued MID
get :: IncrMap a -> Int -> Maybe a
get (k, m) mid = Map.lookup mid m
