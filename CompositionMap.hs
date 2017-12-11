module CompositionMap (MelodyMap, CompositionMap) where

import Music
import IncrMap

type MelodyMap      = IncrMap Melody
type CompositionMap = IncrMap Composition