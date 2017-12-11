-- module CompositionMap (CompositionMap, empty, add, updateWith, get) where
module CompositionMap (MelodyMap, CompositionMap) where

import Music
import IncrMap

import Data.Map (Map)
import qualified Data.Map as Map

-- type CompositionMap = (Int, Map Int Composition)

type MelodyMap      = IncrMap Melody
type CompositionMap = IncrMap Composition