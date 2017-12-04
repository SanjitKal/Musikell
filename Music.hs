{-# LANGUAGE GADTs, FlexibleInstances #-}

module Music where

import Data.List
import qualified Data.List as List
import Euterpea
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

import Test.QuickCheck (Arbitrary(..),Gen(..),Property(..),OrderedList(..),
                        forAll,frequency,elements,sized,oneof,(==>),collect,
                        quickCheck,sample,choose,quickCheckWith,
                        classify,stdArgs,maxSuccess)
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

-- UNARY COMPOSITION OPERATIONS:
setTempo :: Composition -> Rational -> Composition
setTempo c t = undefined

modifyTempo :: Composition -> Rational -> Composition
modifyTempo c t = undefined

transpose :: Composition -> Int -> Composition
transpose c t = undefined

setInstrument :: Composition -> InstrumentName -> Composition
setInstrument c i = undefined

reverse :: Composition -> Composition
reverse (Melody tempo trans m) = Melody tempo trans $ List.reverse m

splitAt :: Composition -> Int -> (Maybe Composition, Maybe Composition)
splitAt c i = undefined

take :: Composition -> Int -> Composition
take c i = undefined

drop :: Composition -> Int -> Composition
drop c i = undefined

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
    arbitrary = frequency [ (1, rest),
                            (9, sound)] where
        rest  = liftM Rest (arbitrary :: Gen Rational)
        sound = liftM3 (\d pc o -> Note d ((pc, o :: Octave) :: Pitch))
                        (elements [x,y..z])
                        (elements [Aff, Af, A, As, Ass, Cff, Cf, C, Cs, Css])
                        (elements [1..8])
        x = 0.05
        y = 0.1
        z = 1.5
    shrink pc = [pc]

instance Arbitrary Note where
    arbitrary = liftM N notes where
        notes = liftM2 (,)
                        (arbitrary :: Gen (Primitive Pitch))
                        (arbitrary :: Gen InstrumentName)

    shrink n = [n]

instance Arbitrary Chord where
    arbitrary = liftM Chord $ (arbitrary :: Gen [Note])

    shrink (Chord l) = liftM Chord $ shrink l

instance Arbitrary Composition where
    arbitrary = liftM3 Melody (arbitrary :: Gen Rational) (arbitrary :: Gen Int) (arbitrary :: Gen [Chord])

    shrink (Melody tempo trans m) = liftM (Melody tempo trans) $ shrink m

