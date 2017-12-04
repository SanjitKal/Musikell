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

-- | A Melody consists of a tempo, a transpose, and a list of Chords, which are
--      played in sequence
data Melody = Melody Rational Int [Chord] deriving (Show, Eq)

-- | A Composition consists of a list of Melodies, which are played together at
--      the same time
data Composition = Composition [Melody] deriving (Show, Eq) -- stack of melodies

-- | A Composition consists of a tempo (a Rational number, the tempo of the
--      underlying melody), a transpose (an Int, the transpose of the
--      underlying melody's notes), and a list of Chords, which are played
--      in succession of each other from the head to the tail.
-- data Composition = Melody Rational Int [Chord] deriving (Show, Eq)

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
    toMusicPitch (Chord [])  = Prim $ Rest 0
    toMusicPitch (Chord (h:t)) = toMusicPitch h :=: toMusicPitch (Chord t)

-- | Playable [Chord] converts a list of Chords into a sequence of playable
--      Music Pitchs (which themselves are converted from Chords)
instance Playable [Chord] where
    toMusicPitch = foldr (\c comp -> toMusicPitch c :+: comp) (Prim (Rest 1))

instance Playable Melody where
    toMusicPitch (Melody tempo trans m) = wrapMusic baseMelody controls where
        baseMelody  = toMusicPitch m
        controls    = [(Tempo tempo), (Transpose trans)]
        wrapMusic b = foldr (\c m -> Modify c m) b

instance Playable Composition where
    toMusicPitch (Composition [])    = Prim $ Rest 0
    toMusicPitch (Composition (h:t)) =
        toMusicPitch h :=: toMusicPitch (Composition t)

-- map transpose over notes (not at composition level)
-- | converts a Composition into a Music Pitch
-- toMusic :: Composition -> Music Pitch
-- toMusic (Melody tempo trans m) = wrapMusic baseMelody controls where
--     baseMelody  = toMusicPitch m
--     controls    = [(Tempo tempo), (Transpose trans)]
--     wrapMusic b = foldr (\c m -> Modify c m) b

-- | mapChord f c Returns a new Chord with the argued function mapped over the
--      argued Chord's inner list
mapChord :: (Note -> Note) -> Chord -> Chord
mapChord f (Chord c) = Chord $ fmap f c

-- UNARY COMPOSITION OPERATIONS:

-- | setTempo t m Returns a new Melody with the same melody and transpose
--      as the argued Melody c, with the tempo set to the argued t
setTempo :: Rational -> Melody -> Melody
setTempo tempo (Melody _ trans m) = Melody tempo trans m

-- | modifyTempo t m Returns a new Melody with the same melody and
--      transpose as the argued Melody c, with the tempo adjusted by the
--      argued amount (positive or negative) t
modifyTempo :: Rational -> Melody -> Melody
modifyTempo d (Melody tempo trans m) = Melody (tempo + d) trans m 

-- | transpose t m Returns a new Melody with the same melody and tempo as
--      the argued Melody c, with the transpose set to the argured t
transpose :: Int -> Melody -> Melody
transpose trans (Melody tempo _ m) = Melody tempo trans m

-- | setInstrument i m Returns a new Melody with the same melody, tempo,
--      and transpose as the argued Melody c, with the instrument of all
--      underlying Notes set to the argument InstrumentName i
setInstrument :: InstrumentName -> Melody -> Melody
setInstrument i (Melody tempo trans m) = Melody tempo trans $ setI m where
    setI = fmap $ mapChord (\(N (pp, _)) -> N (pp, i))

-- | reverseMelody m Returns a new Melody with the same tempo and transpose as
--      the argued Melody c, with the melody reversed
reverseMelody :: Melody -> Melody
reverseMelody (Melody tempo trans m) = Melody tempo trans $ List.reverse m

-- | reverseComposition c Returns a new Composition with the same tempo and
--      transpose as the argued Composition c, with the melody reversed
reverseComposition :: Composition -> Composition
reverseComposition (Composition ms) = Composition $ map reverseMelody ms

-- | collapseMelody m Returns a new Melody with the same tempo and transpose as
--      the argued Melody m, with the melody consisting of a single chord
--      which is all other chords in the original melody stacked together.
collapseMelody :: Melody -> Melody
collapseMelody (Melody tempo trans m) = Melody tempo trans $ pure $ mconcat m

-- | collapseComposition1 c Returns a new Composition with all Melodies
--      of the argued Composition c collapsed into a single Melody, chord by
--      chord, with the tempo and transpose of the first Melody in c
collapseComposition1 :: Composition -> Composition
collapseComposition1 (Composition ms) = undefined

-- | collapseComposition2 c Returns a new Composition with all Melodies
--      of the argued Composition c collapsed into a single Chord, with the
--      tempo and transpose of the first Melody in c
collapseComposition2 :: Composition -> Composition
collapseComposition2 (Composition ms) =
    collapseComposition1 $ Composition $ map collapseMelody ms

-- | take i m Returns a new Melody with the same tempo and transpose as the
--      argued Melody m, with the melody consisting of its first i chords.
--      If i <= 0, then Nothing is returned
take :: Int -> Melody -> Maybe Melody
take i (Melody tempo trans m) =
    if i <= 0
        then Nothing
        else Just $ Melody tempo trans $ List.take i m

-- | drop i m Returns a new Melody with the same tempo and transpose as the
--      argued Melody m, with the melody consisting of all chords after its
--      first i chords. If i >= length of the melody, then Nothing is returned
drop :: Int -> Melody -> Maybe Melody
drop i (Melody tempo trans m) =
    if i >= length m
        then Nothing
        else Just $ Melody tempo trans $ List.drop i m

-- | splitAt i m Returns a 2-tuple of new Melodies. Each Melody has the same
--      tempo and transpose as the argued Melody m. The first Melody consists of
--      the first i chords of m. The second Melody consists of the remaining
--      chords. If i > length melody, then the second Melody will be Nothing. If
--      i <= 0, then both Melodiess will be Nothing. If either Melody would
--      result as an empty melody, Nothing will be returned in its place.
splitAt :: Int -> Melody -> (Maybe Melody, Maybe Melody)
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

instance Monoid Melody where
    mempty = Melody 1.5 0 []

    (Melody tempo trans c1) `mappend` (Melody _ _ c2) =
        Melody tempo trans $ c1 ++ c2

instance Monoid Composition where
    mempty = Composition []

    (Composition ms1) `mappend` (Composition ms2) = Composition $ ms1 ++ ms2

-- | stack m1 m2 Returns a new Composition with the argued Melodies stacked
--      together
stack :: Melody -> Melody -> Composition
stack m1 m2 = Composition [m1, m2]

-- -- stack c1 c2 returns a new composition with the same tempo and trans
-- --     as c1 and chord progression of the form 
-- --     [c1.1++c2.1,c1.2++c2.2 ... c1.n++c2.n ...]
-- --     where cN.K is the Kth chord of the Nth composition
-- --     If length c1 < length c2, excess c2 will be truncated  
-- --     If length c1 > length c2, the following chord progression forms
-- --     [c1.1++c2.1,c1.2++c2.2 ... c1.n++c2.n, c1.(n+1)++c2.1,c1.(n+2)++c2.2 ...]
-- stack :: Composition -> Composition -> Composition
-- stack (Melody tempo trans c1) (Melody _ _ c2) =
--     Melody tempo trans $ zipWith mappend c1 (extend c1 c2) where
--         extend l1 = List.take (length l1) . cycle

-- -- stack c1 c2 returns a new composition with the same tempo and trans
-- --      as c1 and chord progression of the form
-- --      [c1.1++c2.1,c1.2++c2.2 ... c1.n++c2.n ... ]
-- --      where CN.K is the Kth chord of the Nth composition
-- --      If length c1 != length c2, the longer composition is truncated
-- stack2 :: Composition -> Composition -> Composition
-- stack2 (Melody tempo trans c1) (Melody _ _ c2) =
--     Melody tempo trans $ zipWith mappend c1 c2

-- -- stack c1 c2 returns a new composition with the same tempo and trans
-- --      as c1 and chord progression of the form
-- --      [c1.1++c2.1,c1.2++c2.2 ... c1.n++c2.n ...]
-- --      where CN.K is the Kth chord of the Nth composition
-- --      If length c1 < length c2, excess c2 will be truncated
-- --      if length c1 > length c1, excess c1 will be appended to end
-- stack3 :: Composition -> Composition -> Composition
-- stack3 (Melody tempo trans c1) (Melody _ _ c2) =
--     Melody tempo trans (comb c1 c2) where
--         comb (x:xs) (y:ys) = (mappend x y) : comb xs ys
--         comb [] _          = []
--         comb xs []         = xs

-- -- intersperse c1 c2 returns a new composition with the same tempo and trans
-- --      as c1 and chord progression of the form
-- --      [c1.1,c2.2,c1.2,c2.2 ... c1.n,c2.n ...] 
-- --      where CN.K is the Kth chord of the Nth composition
-- --      If length c1 != length c2, the longer composition is truncated
-- intersperse1 :: Composition -> Composition -> Composition
-- intersperse1 (Melody tempo trans c1) (Melody _ _ c2) =
--     Melody tempo trans (inter1 c1 c2) where
--         inter1 (x:xs) (y:ys) = x : y : inter1 xs ys
--         inter1 [] _          = []
--         inter1 _ []          = []

-- -- intersperse2 c1 c2 returns a new composition with the same tempo and trans
-- --      as c1 and chord progression of the form
-- --      [c1.1,c2,c1.2,c2 ... c1.n,c2 ...] 
-- --      where CN.K is the Kth chord of the Nth composition
-- intersperse2 :: Composition -> Composition -> Composition
-- intersperse2 (Melody tempo trans c1) (Melody _ _ c2) =
--     Melody tempo trans (inter2 c1 c2) where
--         inter2 (x:xs) ys = [x] ++ ys ++ inter2 xs ys
--         inter2 [] _     = []

-- -- intersperse2n c1 c2 returns a new composition with the same tempo and trans
-- --      as c1 and chord progression of the form
-- --      [c1.1,c1.2 ... c1.n,c2,c1.(n+1),c1.(n+2), ... c1.2n,c2 ...] 
-- --      where CN.K is the Kth chord of the Nth composition
-- --      If (mod c1 n != 0), excess c1 is appended to end of comp without c2
-- intersperse2n :: Int -> Composition -> Composition -> Composition
-- intersperse2n n (Melody tempo trans c1) (Melody _ _ c2) =
--     Melody tempo trans (inter3 c1 c2 n num) where
--         num = mod (length c1) n
--         inter3 _ _ _ 0     = []
--         inter3 xs ys n num =
--             (List.take n xs) ++ ys ++ inter3 (List.drop n xs) ys n (num-1)

-- Arbitrary instances

instance Arbitrary InstrumentName where
    arbitrary = elements instruments where
        instruments = [ AcousticGrandPiano
                      -- , Harmonica
                      -- , ElectricGrandPiano
                      -- , HonkyTonkPiano
                      -- , Accordion
                      -- , ChorusedPiano
                      -- , SlapBass2
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

instance Arbitrary Melody where
    arbitrary = liftM3 Melody
                    (liftM abs (arbitrary :: Gen Rational)) -- tempo
                    (liftM abs (arbitrary :: Gen Int))      -- transpose
                    (arbitrary :: Gen [Chord])              -- melody

    shrink (Melody tempo trans m) = liftM (Melody tempo trans) $ shrink m

instance Arbitrary Composition where
    arbitrary = liftM Composition $ randoMelody where
        randoMelody = frequency [ (1, oneMelody)
                                , (1, twoMelodies)
                                ]
        oneMelody   = liftM  (\n        -> [n])
                             (arbitrary :: Gen Melody)
        twoMelodies = liftM2 (\n1 n2    -> [n1, n2])
                             (arbitrary :: Gen Melody)
                             (arbitrary :: Gen Melody)

-- Random music :)
randomMelodyN :: Int -> Rational -> Int -> IO Melody
randomMelodyN n tempo trans = do
    m' <- generate m
    play $ toMusicPitch m'
    return m'

    where m = liftM (Melody tempo trans)
                (vectorOf n (arbitrary :: Gen Chord))
