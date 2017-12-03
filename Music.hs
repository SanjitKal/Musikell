{-# LANGUAGE GADTs #-}

module Music where


import Euterpea
import Data.Text (splitOn, pack, unpack)
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

type Note = PitchClass

data Chord = Chord [Note]

data Composition = Melody [Chord]

class Playable a where
    toMusicPitch :: a -> Music Pitch

instance Playable Chord where
    toMusicPitch (Chord [])  = Prim $ Rest 1
    toMusicPitch (Chord (h:t)) = (Prim $ toPrimitive 1 h 4) :=: toMusicPitch (Chord t)

toMusic :: Composition -> Music Pitch
toMusic (Melody m) = foldr (\n comp -> toMusicPitch n :+: comp) (Prim (Rest 1)) m

instance Monoid Composition where
    mempty = Melody []
    (Melody c1) `mappend` (Melody c2) = Melody $ c1 ++ c2

instance Monoid Chord where
    mempty = Chord []
    (Chord c1) `mappend` (Chord c2) = Chord $ c1 ++ c2

split :: String -> String -> [String]
split d s = map unpack (splitOn (pack d) (pack s))

toComposition :: [String] -> Composition
toComposition = Melody . map toChord

toChord :: String -> Chord
toChord = Chord . map toNote . split ","

toPitch :: Note -> Int -> Pitch
toPitch n o = (n, o :: Octave) :: Pitch

toPrimitive :: Rational -> Note -> Int -> Primitive Pitch
toPrimitive d n o = Note d (toPitch n o)

toNote :: String -> Note
toNote "cff" =  Cff
toNote "cf"  =  Cf
toNote "c"   =  C
toNote "cs"  =  Cs
toNote "css" =  Css
toNote "dff" =  Dff
toNote "df"  =  Df
toNote "d"   =  D
toNote "ds"  =  Ds
toNote "dss" =  Dss
toNote "eff" =  Eff
toNote "ef"  =  Ef
toNote "e"   =  E
toNote "es"  =  Es
toNote "ess" =  Ess
toNote "fff" =  Fff
toNote "ff"  =  Ff
toNote "f"   =  F
toNote "fs"  =  Fs
toNote "fss" =  Fss
toNote "gff" =  Gff
toNote "gf"  =  Gf
toNote "g"   =  G
toNote "gs"  =  Gs
toNote "gss" =  Gss
toNote "aff" =  Aff
toNote "af"  =  Af
toNote "a"   =  A
toNote "as"  =  As
toNote "ass" =  Ass
toNote "bff" =  Bff
toNote "bf"  =  Bf
toNote "b"   =  B
toNote "bs"  =  Bs
toNote "bss" =  Bss
toNote _     = C 

-- data Composition = Harmony [Note] | Melody [Note]

-- type Composition = [Note]

-- toMusic :: (Music Pitch -> Music Pitch -> Music Pitch) -> Composition -> Music Pitch
-- toMusic comb = foldr (\n comp -> toMusicPitch n `comb` comp) (Prim (Rest 1))

-- For Note instead of Prim
-- data Composition =
--     Harmony ([Note], [Mod])
--     | Melody ([Note], [Mod])



-- Prim (Primitive a)   
-- (Music a) :+: (Music a) infixr 5     
-- (Music a) :=: (Music a) infixr 5     
--  Modify Control (Music a)

-- :t play = (NFData a, ToMusic1 a) => Music a -> IO ()

-- toMusic :: Composition -> Music Pitch
-- toMusic Harmony = undefined


-- Compositional characteristics of a sound
data Mod =
    Tempo Int -- Time between note hits
    | Duration Int -- quarter note, half note, whole note, etc

-- We are also considering adding a Mod type that alters the structural
-- characteristics of a sound (different instruments)

-- Map from String IDs to Sounds to keep track whats playing
-- type World = Map String Composition 

-- -- Add a composition to the world (of currently stored sounds)
-- add :: String -> Composition -> World -> World 
-- add = Map.insert

-- -- Remove a composition from the S
-- remove :: String -> World -> World
-- remove = Map.delete

-- -- Modify a composition in the world with the provided mod
-- modify :: String -> Mod -> World -> World
-- modify id mod world = case Map.lookup id world of
--                            Nothing -> world
--                            Just comp -> Map.insert id (applyMod mod comp) world

-- -- Modify a composition
-- applyMod :: Mod -> Composition -> Composition
-- applyMod (Tempo _) h@(Harmony _) = h
-- applyMod t@(Tempo _) (Melody (m, ms)) = Melody (m, t:ms)
-- applyMod d@(Duration _) c@(Harmony (h, ms)) = Harmony (h, d:ms)
-- applyMod d@(Duration _) c@(Melody (m, ms)) = Melody (m, d:ms)

-- Play the sounds
-- play :: Composition -> IO ()
-- play (Melody  (ns, ms)) = undefined
-- play (Harmony (ns, ms)) = undefined


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
