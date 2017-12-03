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

updateWith :: CompositionMap -> (Composition -> Composition-> Composition) -> Int -> Composition -> Maybe CompositionMap
updateWith (k, m) f cid c =
    if Map.member cid m
        then Just (k, Map.insertWith f cid c m)
        else Nothing

get :: CompositionMap -> Int -> Maybe Composition
get (k, m) cid = Map.lookup cid m
