{-# LANGUAGE GADTs #-}

module Music where


import Euterpea
import Text.Read (readMaybe)
import Data.Text (splitOn, pack, unpack)
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

data Note = N (Primitive Pitch, InstrumentName)

data Chord = Chord [Note]

data Composition = Melody Rational Int [Chord]

class Playable a where
    toMusicPitch :: a -> Music Pitch

instance Playable Note where
    toMusicPitch (N (pp, instr)) = Modify (Instrument instr) $ Prim pp 

instance Playable Chord where
    toMusicPitch (Chord [])  = Prim $ Rest 1
    toMusicPitch (Chord (h:t)) = toMusicPitch h :=: toMusicPitch (Chord t)

toMusic :: Composition -> Music Pitch
toMusic (Melody tempo trans m) = Modify (Tempo tempo) (Modify (Transpose trans) (foldr (\c comp -> toMusicPitch c :+: comp) (Prim (Rest 1)) m))

instance Monoid Composition where
    mempty = Melody 0 0 []
    (Melody tempo trans c1) `mappend` (Melody _ _ c2) = Melody tempo trans (c1 ++ c2)

instance Monoid Chord where
    mempty = Chord []
    (Chord c1) `mappend` (Chord c2) = Chord $ c1 ++ c2


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
