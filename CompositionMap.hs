module CompositionMap (CompositionMap, empty, add, updateWith, get) where

import Music

import Data.Map (Map)
import qualified Data.Map as Map

type CompositionMap = (Int, Map Int Composition)

-- | An empty CompositionMap
empty :: CompositionMap
empty = (1, Map.empty)

-- | Add the argued Composition to the argued CompositionMap. Return the ID of
--  the added Composition along with the updated CompositionMap
add :: CompositionMap -> Composition -> (Int, CompositionMap)
add (k, m) c = (k, (k + 1, Map.insert k c m))

updateWith :: Int -> (Composition -> Composition) -> CompositionMap -> Maybe CompositionMap
updateWith cid f (k, m) =
    case Map.lookup cid m of
        Just c -> Just (k, Map.insert cid (f c) m)
        _      -> Nothing

get :: CompositionMap -> Int -> Maybe Composition
get (k, m) cid = Map.lookup cid m
