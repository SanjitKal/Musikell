module CompositionMap (CompositionMap, empty, add, update, get) where

import Music

import Data.Map (Map)
import qualified Data.Map as Map

data CompositionMap = CM :: Playable a => (Int, Map Int (Composition a))

empty :: CompositionMap
empty = (1, Map.empty)

add :: CompositionMap -> Composition -> (Int, CompositionMap)
add (k, m) v = (k, (k + 1, Map.insert k v m))

update :: Playbalbe a => CompositionMap -> Int -> a -> Maybe CompositionMap
update (k, m) cid n = if Map.member cid m
                        then Just (k, Map.insertWith (<>) cid n m)
                        else Nothing

get :: CompositionMap -> Int -> Maybe Composition
get (k, m) cid = Map.lookup cid m
