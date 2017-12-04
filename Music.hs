{-# LANGUAGE GADTs, FlexibleInstances #-}

module Music where

import Data.List
import qualified Data.List as List
import Euterpea
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

import Test.QuickCheck (Arbitrary(..),Gen(..),Property(..),OrderedList(..),
                        forAll,frequency,elements,sized,oneof,(==>),collect,
                        quickCheck,sample,choose,quickCheckWith,
                        classify,stdArgs,maxSuccess,vectorOf)
import Control.Monad (liftM,liftM2,liftM3)

data Note = N (Primitive Pitch, InstrumentName)

data Chord = Chord [Note]

--   Composition = Melody Tempo Transpose [Chord]
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

-- instance Monad Chord where
--     return = Chord

--     (Chord c) >>= f = Chord $ f c

-- instance Applicative Chord where
--     pure  = return
--     (<*>) = ap

-- instance Functor Chord where
--     fmap = liftM

mapChord :: (Note -> Note) -> Chord -> Chord
mapChord f (Chord c) = Chord $ fmap f c

-- UNARY COMPOSITION OPERATIONS:

-- | setTempo t c Returns a new Composition with the same melody and transpose
--      as the argued Compostion c, with the tempo set to the argued t
setTempo :: Rational -> Composition -> Composition
setTempo tempo (Melody _ trans m) = Melody tempo trans m

-- | modifyTempo t c Returns a new Composition with the same melody and
--      transpose as the argued Composition c, with the tempo adjusted by the
--      argued amount (positive or negative) t
modifyTempo :: Rational -> Composition -> Composition
modifyTempo d (Melody tempo trans m) = Melody (tempo + d) trans m 

-- | transpose t c Returns a new Composition with the same melody and tempo as
--      the argued Composition c, with the transpose set to the argured t
transpose :: Int -> Composition -> Composition
transpose trans (Melody tempo _ m) = Melody tempo trans m

-- | setInstrument i c Returns a new Composition with the same melody, tempo,
--      and transpose as the argued Composition c, with the instrument of all
--      underlying Notes set to the argument InstrumentName i
setInstrument :: InstrumentName -> Composition -> Composition
setInstrument i (Melody tempo trans m) = Melody tempo trans $ setI m where
    setI = fmap $ mapChord (\(N (pp, _)) -> N (pp, i))

-- | reverse c Returns a new Composition with the same tempo and transpose as
--      the argued Composition c, with the melody reversed
reverse :: Composition -> Composition
reverse (Melody tempo trans m) = Melody tempo trans $ List.reverse m

-- | splitAt i c Returns a 2-tuple of new Compositions. Each Comoposition has
--      the same tempo and transpose as the argued Composition c. The melody of
--      the first Composition consists of the first i chords of the melody of
--      the argued Composition c. The melody of the second Composition consists
--      of the remaining chords. If i > length melody, then the second
--      Compsition will be Nothing. If i <= 0, then both compositions will be
--      Nothing. If either Composition would result as an empty melody, Nothing
--      will be returned in its place.
splitAt :: Int -> Composition -> (Maybe Composition, Maybe Composition)
splitAt i (Melody tempo trans m) =
    case (c1, c2) of
        ([], _)  -> (Nothing, Nothing)
        (m1, []) -> (justMelody m1, Nothing)
        (m1, m2) -> (justMelody m1, justMelody m2)
    where
        (c1, c2)   = List.splitAt i m
        justMelody = Just . Melody tempo trans

-- | take i c Returns a new Composition with the same tempo and transpose as the
--      argued Composition c, with the melody consisting of its first i chords.
--      If i <= 0, then Nothing is returned
take :: Int -> Composition -> Maybe Composition
take i (Melody tempo trans m) =
    if i <= 0
        then Nothing
        else Just $ Melody tempo trans $ List.take i m

-- | drop i c Returns a new Composition with the same tempo and transpose as the
--      argued Composition c, with the melody consisting of all chords after its
--      first i chords. If i >= length of the melody, then Nothing is returned
drop :: Int -> Composition -> Maybe Composition
drop i (Melody tempo trans m) =
    if i >= length m
        then Nothing
        else Just $ Melody tempo trans $ List.drop i m

-- | collapse c Returns a new Composition with the same tempo and transpose as
--      the argued Composition c, with the melody consisting of a single chord
--      which is all other chords in the original melody stacked together.
collapse :: Composition -> Composition
collapse (Melody tempo trans m) = Melody tempo trans $ pure $ mconcat m

-- BINARY COMPOSITION OPERATIONS:

instance Monoid Chord where
    mempty = Chord []
    (Chord c1) `mappend` (Chord c2) = Chord $ c1 ++ c2

instance Monoid Composition where
    mempty = Melody 0 0 []
    (Melody tempo trans c1) `mappend` (Melody _ _ c2) = Melody tempo trans (c1 ++ c2)

stack :: Composition -> Composition -> Composition
stack (Melody tempo trans c1) (Melody _ _ c2) = Melody tempo trans $ zipWith mappend c1 (extend c1 c2)
    where extend l1 = List.take (length l1) . cycle

-- just zipWith <>
stack2 :: Composition -> Composition -> Composition
stack2 c1 c2 = undefined

-- just put c2 once at the beginning of c1 then return the rest of c1 unmodified
stack3 :: Composition -> Composition -> Composition
stack3 c1 c2 = undefined

-- ChordA1, ChordA2, ChordB1, ChordB2, ...
intersperse1 :: Composition -> Composition -> Composition
intersperse1 c1 c2 = undefined

-- ChordA1, c2, ChordB1, c2, ...
intersperse2 :: Composition -> Composition -> Composition
intersperse2 c1 c2 = undefined

-- ChordA1, ChordB1, ..., Chordn1, c2, Chord(n+1)1, ..., Chord(2n)1, c2, ...
intersperse2n :: Composition -> Composition -> Int -> Composition
intersperse2n c1 c2 n = undefined

-- Arbitrary instances

instance Arbitrary InstrumentName where
    arbitrary = elements instruments where
        instruments = [ AcousticGrandPiano
                      , Harmonica
                      , ElectricGrandPiano
                      , HonkyTonkPiano
                      , Accordion
                      , ChorusedPiano
                      , SlapBass2
                      , VoiceOohs
                      ]

    shrink i = [i]

instance Arbitrary (Primitive Pitch) where
    arbitrary = frequency [ (0, rest),
                            (10, sound)] where
        rest  = liftM Rest (arbitrary :: Gen Rational)
        sound = liftM (\pc -> Note 1.0 ((pc, 4 :: Octave) :: Pitch))
                        (elements [ Aff, Af, A, As, Ass,
                                    Bff, Bf, B, Bs, Bss,
                                    Cff, Cf, C, Cs, Css,
                                    Dff, Df, D, Ds, Dss,
                                    Eff, Ef, E, Es, Ess,
                                    Fff, Ff, F, Fs, Fss,
                                    Gff, Gf, G, Gs, Gss])
    shrink pc = [pc]

instance Arbitrary Note where
    arbitrary = liftM N notes where
        notes = liftM2 (,)
                        (arbitrary :: Gen (Primitive Pitch))
                        (arbitrary :: Gen InstrumentName)

    shrink n = [n]

instance Arbitrary Chord where
    arbitrary = liftM Chord $ randoChord where
        randoChord = frequency [ (1, liftM  (\n        -> [n])          (arbitrary :: Gen Note))
                               , (1, liftM2 (\n1 n2    -> [n1, n2])     (arbitrary :: Gen Note) (arbitrary :: Gen Note))
                               , (1, liftM3 (\n1 n2 n3 -> [n1, n2, n3]) (arbitrary :: Gen Note) (arbitrary :: Gen Note) (arbitrary :: Gen Note))
                               ]

    shrink (Chord l) = liftM Chord $ shrink l

instance Arbitrary Composition where
    arbitrary = liftM3 Melody (arbitrary :: Gen Rational) (arbitrary :: Gen Int) (arbitrary :: Gen [Chord])

    shrink (Melody tempo trans m) = liftM (Melody tempo trans) $ shrink m
