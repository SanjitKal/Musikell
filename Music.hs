module Music where

import Data.Map (Map)
import qualified Data.Map as Map

import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

type Note = Char
data Composition =
    Harmony ([Note], [Mod])
    | Melody ([Note], [Mod])

-- stuff that doesn't change the characteristics of the sound
-- just changes how the sound(s) are played together
data Mod =
    Tempo Int
    | Duration Int -- quarter note, half note, whole note, etc

type S = Map String Composition

-- add a composition to the S (currently stored sounds)
add :: String -> Composition -> S -> S 
add = undefined

-- remove a composition from the S
remove :: String -> S -> S
remove = undefined

-- modify a composition in the S with the argued Mod
modify :: String -> Mod -> S -> S
modify = undefined

-- modify a composition
applyMod :: Mod -> Composition -> Composition
applyMod (Tempo _) h@(Harmony _) = h
applyMod t@(Tempo _) (Melody (m, ms)) = Melody (m, t:ms)

-- play the sounds
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
