module CompositionMap (CompositionMap, empty, add, update, get) where

import Music

import Data.Map (Map)
import qualified Data.Map as Map

type CompositionMap = (Int, Map Int (String, Composition))

empty :: CompositionMap
empty = (1, Map.empty)

add :: CompositionMap -> (String, Composition) -> (Int, CompositionMap)
add (k, m) v = (k, (k + 1, Map.insert k v m))

update :: CompositionMap -> Int -> Note -> Maybe CompositionMap
update (k, m) cid n = if Map.member cid m
                        then Just (k, Map.insertWith (\(_, nn) (t, ns) -> (t, ns ++ nn)) cid ("", [n]) m)
                        else Nothing

get :: CompositionMap -> Int -> Maybe (String, Composition)
get (k, m) cid = Map.lookup cid m
