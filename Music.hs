{-# LANGUAGE GADTs, FlexibleInstances #-}

module Music where

import Data.List
import qualified Data.List as List
import Euterpea
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

import Test.QuickCheck (Arbitrary(..),Gen(..),Property(..),OrderedList(..),
                        forAll,frequency,elements,sized,oneof,(==>),collect,
                        quickCheck,sample,choose,quickCheckWith, generate,
                        classify,stdArgs,maxSuccess,vectorOf)
import Control.Monad (liftM,liftM2,liftM3)

-- | A Note consists of a Primitive Pitch (the note to play) and an
--      InstrumentName (the type of instrument with which to play the note).
--      These types come from the import library Euterpea.
data Note = N (Primitive Pitch, InstrumentName) deriving (Show, Eq)

-- | A Chord consists of a list of Notes, which are played together at the same
--      time.
data Chord = Chord [Note] deriving (Show, Eq)

-- | A Composition consists of a tempo (a Rational number, the tempo of the
--      underlying melody), a transpose (an Int, the transpose of the
--      underlying melody's notes), and a list of Chords, which are played
--      in succession of each other from the head to the tail.
data Composition = Melody Rational Int [Chord] deriving (Show, Eq)

-- | A Playable type can be converted into a Music Pitch, which is a type from
--      the imported Euterpea, which can be played via the machine's speakers
class Playable a where
    toMusicPitch :: a -> Music Pitch

-- | Playable Note converts the data type Note from above into a Music Pitch
--      which can be argued to Euterpea's play function and therefore played
--      out of the machine's speakers.
instance Playable Note where
    toMusicPitch (N (pp, instr)) = Modify (Instrument instr) $ Prim pp 

-- | Playble Chord converts the data type Chord from above into a Music Pitch
--      which can be argued to Euterpea's play function and therefore played
--      out of the machine's speakers.
instance Playable Chord where
    toMusicPitch (Chord [])  = Prim $ Rest 1
    toMusicPitch (Chord (h:t)) = toMusicPitch h :=: toMusicPitch (Chord t)

-- | Playable [Chord] converts a list of Chords into a sequence of playable
--      Music Pitchs (which themselves are converted from Chords)
instance Playable [Chord] where
    toMusicPitch = foldr (\c comp -> toMusicPitch c :+: comp) (Prim (Rest 1))

-- | converts a Composition into a Music Pitch
toMusic :: Composition -> Music Pitch
toMusic (Melody tempo trans m) = wrapMusic baseMelody controls where
    baseMelody  = toMusicPitch m
    controls    = [(Tempo tempo), (Transpose trans)]
    wrapMusic b = foldr (\c m -> Modify c m) b

-- | mapChord f c Returns a new Chord with the argued function mapped over the
--      argued Chord's inner list
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

-- | collapse c Returns a new Composition with the same tempo and transpose as
--      the argued Composition c, with the melody consisting of a single chord
--      which is all other chords in the original melody stacked together.
collapse :: Composition -> Composition
collapse (Melody tempo trans m) = Melody tempo trans $ pure $ mconcat m

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

-- BINARY COMPOSITION OPERATIONS:

instance Monoid Chord where
    mempty = Chord []

    (Chord c1) `mappend` (Chord c2) = Chord $ c1 ++ c2

instance Monoid Composition where
    mempty = Melody 0 0 []

    (Melody tempo trans c1) `mappend` (Melody _ _ c2) =
        Melody tempo trans $ c1 ++ c2

stack :: Composition -> Composition -> Composition
stack (Melody tempo trans c1) (Melody _ _ c2) =
    Melody tempo trans $ zipWith mappend c1 (extend c1 c2) where
        extend l1 = List.take (length l1) . cycle

-- just zipWith <>
stack2 :: Composition -> Composition -> Composition
stack2 (Melody tempo trans c1) (Melody _ _ c2) =
    Melody tempo trans $ zipWith mappend c1 c2

-- just put c2 once at the beginning of c1 then return the rest of c1 unmodified
-- if c2 is longer than c1, truncate c2
stack3 :: Composition -> Composition -> Composition
stack3 (Melody tempo trans c1) (Melody _ _ c2) =
    Melody tempo trans (comb c1 c2) where
        comb (x:xs) (y:ys) = (mappend x y) : comb xs ys
        comb [] _          = []
        comb xs []         = xs

-- ChordA1, ChordA2, ChordB1, ChordB2, ...
intersperse1 :: Composition -> Composition -> Composition
intersperse1 (Melody tempo trans c1) (Melody _ _ c2) =
    Melody tempo trans (inter1 c1 c2) where
        inter1 (x:xs) (y:ys) = x : y : inter1 xs ys
        inter1 [] _          = []
        inter1 _ []          = []

-- ChordA1, c2, ChordB1, c2, ...
intersperse2 :: Composition -> Composition -> Composition
intersperse2 (Melody tempo trans c1) (Melody _ _ c2) =
    Melody tempo trans (inter2 c1 c2) where
        inter2 (x:xs) ys = [x] ++ ys ++ inter2 xs ys
        inter2 [] _     = []

-- ChordA1, ChordB1, ..., Chordn1, c2, Chord(n+1)1, ..., Chord(2n)1, c2, ...
intersperse2n :: Int -> Composition -> Composition -> Composition
intersperse2n n (Melody tempo trans c1) (Melody _ _ c2) =
    Melody tempo trans (inter3 c1 c2 n num) where
        num = mod (length c1) n
        inter3 _ _ _ 0     = []
        inter3 xs ys n num =
            (List.take n xs) ++ ys ++ inter3 (List.drop n xs) ys n (num-1)

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
        randoChord = frequency [ (1, oneNote)
                               , (1, twoNotes)
                               , (1, threeNotes)
                               ]
        oneNote    = liftM  (\n        -> [n])
                            (arbitrary :: Gen Note)
        twoNotes   = liftM2 (\n1 n2    -> [n1, n2])
                            (arbitrary :: Gen Note)
                            (arbitrary :: Gen Note)
        threeNotes = liftM3 (\n1 n2 n3 -> [n1, n2, n3])
                            (arbitrary :: Gen Note)
                            (arbitrary :: Gen Note)
                            (arbitrary :: Gen Note)

    shrink (Chord l) = liftM Chord $ shrink l

instance Arbitrary Composition where
    arbitrary = liftM3 Melody
                    (liftM abs (arbitrary :: Gen Rational)) -- tempo
                    (liftM abs (arbitrary :: Gen Int))      -- transpose
                    (arbitrary :: Gen [Chord])              -- melody

    shrink (Melody tempo trans m) = liftM (Melody tempo trans) $ shrink m

-- Random music :)
nRandomChords :: Int -> Rational -> Int -> IO ()
nRandomChords n tempo trans = do
    c' <- generate c
    play $ toMusic c'

    where c = liftM (Melody tempo trans)
                (vectorOf n (arbitrary :: Gen Chord))   -- melody
