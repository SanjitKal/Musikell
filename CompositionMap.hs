module CompositionMap (CompositionMap, empty, add, update, get) where

import Music

import Data.Map (Map)
import qualified Data.Map as Map

data CompositionMap where
    CM :: Playable a => (Int, Map Int (Composition a))

-- | An empty CompositionMap
empty :: CompositionMap
empty = (1, Map.empty)

-- | Add the argued Composition to the argued CompositionMap. Return the ID of
--  the added Composition along with the updated CompositionMap
add :: Playable a => CompositionMap -> Composition a -> (Int, CompositionMap)
add (k, m) c = (k, (k + 1, Map.insert k c m))

updateWith :: Playbalbe a => CompositionMap -> (Composition a -> Compostion a -> Composition a) -> Int -> Composition a -> Maybe CompositionMap
updateWith (k, m) f cid c =
    if Map.member cid m
        then Just (k, Map.insertWith f cid n m)
        else Nothing

get :: Playable a => CompositionMap -> Int -> Maybe Composition a
get (k, m) cid = Map.lookup cid m
