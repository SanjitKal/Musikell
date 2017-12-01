module Music where

import Data.Map (Map)
import qualified Data.Map as Map

import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

type Note = Char
data Composition =
    Harmony ([Note], [Mod])
    | Melody ([Note], [Mod])


-- Music a
-- type Pitch = (PitchClass, Octave)
-- type Octave = Int

-- Compositional characteristics of a sound
data Mod =
    Tempo Int -- Time between note hits
    | Duration Int -- quarter note, half note, whole note, etc

-- We are also considering adding a Mod type that alters the structural
-- characteristics of a sound (different instruments)

-- Map from String IDs to Sounds to keep track whats playing
type World = Map String Composition 

-- Add a composition to the world (of currently stored sounds)
add :: String -> Composition -> World -> World 
add = Map.insert

-- Remove a composition from the S
remove :: String -> World -> World
remove = Map.delete

-- Modify a composition in the world with the provided mod
modify :: String -> Mod -> World -> World
modify id mod world = case Map.lookup id world of
                           Nothing -> world
                           Just comp -> Map.insert id (applyMod mod comp) world

-- Modify a composition
applyMod :: Mod -> Composition -> Composition
applyMod (Tempo _) h@(Harmony _) = h
applyMod t@(Tempo _) (Melody (m, ms)) = Melody (m, t:ms)
applyMod d@(Duration _) c@(Harmony (h, ms)) = Harmony (h, d:ms)
applyMod d@(Duration _) c@(Melody (m, ms)) = Melody (m, d:ms)

-- Play the sounds
play :: Composition -> IO ()
play (Melody  (ns, ms)) = undefined
play (Harmony (ns, ms)) = undefined


-- $ musikell
-- > m a,b,c 120
-- > show sounds
-- { s1: Melody [a, b, c], 120 }
-- > t s1 100
-- > t 1000

-- m -> melody 
-- n -> note
-- h -> harmony
-- t -> tempo (given a sound)
-- d -> do whatever


-- Tests
tAdd :: Test
tAdd = "add" ~:
    TestList
    [ "add to empty" ~: True ~?= True
    , "add to nonempty, new id" ~: True ~?= True 
    , "add to nonempty, existing id" ~: True ~?= True 
    ]

tRemove :: Test
tRemove = "remove" ~:
    TestList
    [ "remove from empty" ~: True ~?= True 
    , "remove from nonempty, key exists" ~: True ~?= True 
    , "remove from nonempty, key does notexist" ~: True ~?= True 
    ]

tModify :: Test
tModify = "modify" ~:
    TestList
    [ "modify empty" ~: True ~?= True 
    , "modify nonempty, key exists" ~: True ~?= True 
    , "modify nonempty, key does notexist" ~: True ~?= True 
    , "inverse modify" ~: True ~?= True 
    ]

tApplyMod :: Test
tApplyMod = "applyMod" ~:
    TestList
    [ "apply Tempo to Harmony" ~: True ~?= True
    , "apply Tempo to Melody" ~: True ~?= True
    , "apply Duration to Harmony" ~: True ~?= True
    , "apply Duration to Melody" ~: True ~?= True
    ]

-- How _do_ you test IO? (Play)
