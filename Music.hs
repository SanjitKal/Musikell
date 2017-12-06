{-# LANGUAGE GADTs, FlexibleInstances #-}

module Music where

import Data.List
import qualified Data.List as List
import Euterpea
import Data.Monoid
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
data Chord = Chord {notes :: [Note]} deriving (Show, Eq)

-- | A Melody consists of a tempo, a transpose, and a list of Chords, which are
--      played in sequence
data Melody = Melody {temp :: Rational, tran :: Int, chords :: [Chord]}
     deriving (Show, Eq)

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

repl :: Int -> Melody -> Melody
repl n m = if n == 0 then mempty else m <> (repl (n-1) m)

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
--      together.
stack :: Melody -> Melody -> Composition
stack m1 m2 = Composition [m1, m2]

-- stack m1 m2 returns a new composition of the form
--     [m1.1++m2.1,m1.2++m2.2 ... m1.n++m2.n ...]
--     where mN.K is the Kth chord of the Nth melody
--     If length m1 < length m2, excess m2 will be truncated  
--     If length m1 > length m2, the following chord progression forms
--     [m1.1++m2.1,m1.2++m2.2 ... m1.n++m2.n, m1.(n+1)++m2.1,m1.(n+2)++m2.2 ...]
stackCycle :: Melody -> Melody -> Composition
stackCycle m1 m2 = Composition [m1, newm2]
    where newm2 = Melody (temp m2) (tran m2) (extend (chords m1) (chords m2))
          extend l1 = List.take (length l1) . cycle


--THIS FUNCTION MAY DO WHAT STACK DOES.
-- stack m1 m2 returns a new composition of the form
--      [m1.1++m2.1,m1.2++m2.2 ... m1.n++m2.n ... ]
--      where nN.K is the Kth chord of the Nth composition
--      If length m1 != length m2, the longer composition is truncated
stackTruncate :: Melody -> Melody -> Composition
stackTruncate m1 m2 = Composition [newm1, newm2]
    where newm1 = Melody (temp m1) (tran m1) (List.take len (chords m1))
          newm2 = Melody (temp m2) (tran m2) (List.take len (chords m2)) 
          len   = min (length $ chords m1) (length $ chords m2) 

-- stack m1 m2 returns a new composition of the form
--      [m1.1++m2.1,m1.2++m2.2 ... m1.n++m2.n ...]
--      where CN.K is the Kth chord of the Nth composition
--      If length m1 < length m2, excess m2 will be truncated
--      if length m1 > length m1, excess m1 will be appended to end
stackPreserve :: Melody -> Melody -> Composition
stackPreserve m1 m2 = Composition [newm1, newm2]
    where newm1 = Melody (temp m1) (tran m1) (List.take len (chords m1))
          newm2 = Melody (temp m2) (tran m2) (List.take len (chords m2)) 
          len   = length $ chords m1

-- intersperse m1 m2 returns a new composition with the same tempo and trans
--      as m1 and chord progression of the form
--      [m1.1,m2.2,m1.2,m2.2 ... m1.n,m2.n ...] 
--      where CN.K is the Kth chord of the Nth composition
--      If length m1 != length m2, the longer composition is truncated
intersperse1 :: Melody -> Melody -> Composition
intersperse1 m1 m2 = Composition [newm1, newm2]
    where newm1  = Melody (temp m1) (tran m1) (fst tup)
          newm2  = Melody (temp m1) (tran m2) (snd tup)
          tup    = sperse (chords m1) (chords m2)

-- Takes two lists of chords and places rests between notes in each list such
--      that notes in each of the two chords take turns playing without any overlap.
--      A tuple with these modified lists of chords is returned
sperse :: [Chord] -> [Chord] -> ([Chord], [Chord])
sperse c1 c2 = foldr f ([],[]) (zip c1 c2)
    where f = \(a, b) (acc1, acc2) -> (a : gap b : acc1, gap a : b : acc2)

-- Returns a chord containing a rest with a duration of the length of the
--      longest note in the argued chord
gap :: Chord -> Chord
gap c = Chord [N (Rest (clen (notes c)), AcousticGrandPiano)]

-- Returns the duration of a note
getDur :: Note -> Rational
getDur (N (Rest dur, _)) = dur
getDur (N (Note dur _, _)) = dur

-- Returns the length of the longest note in the argued list of notes
clen :: [Note] -> Rational
clen = foldr (\n acc -> max (getDur n) (acc)) 0 
 
seq :: Melody -> Melody -> Composition
seq m1 m2 = Composition [m1, newm2]
    where newm2 = Melody (temp m2) (tran m2) (prerest : (chords m2))
          prerest = Chord [N (Rest dur, AcousticGrandPiano)]
          dur = (foldr (\c acc -> clen (notes c) + acc) 0 (chords m1)) * ratio
          ratio = (temp m2 / temp m1)

-- -- intersperse2 m1 m2 returns a new composition with the same tempo and trans
-- --      as m1 and chord progression of the form
-- --      [m1.1,m2,m1.2,m2 ... m1.n,m2 ...] 
-- --      where CN.K is the Kth chord of the Nth composition
intersperse2 :: Melody -> Melody -> Composition
intersperse2 m1 m2 = Composition [newm1, newm2]
    where newm1 = Melody (temp m1) (tran m1) (fst tup)
          newm2 = Melody (temp m1) (tran m2) (snd tup)
          tup   = sperse2 (chords m1) (chords m2)


-- Takes two lists of chords and places rests betweeen notes in each list such
--      that a chord of the first composition will play followed by the
--      whole second composition and then the second chord of the first composition
--      will play followed by the whole second composition and so on.
sperse2 :: [Chord] -> [Chord] -> ([Chord], [Chord])
sperse2 c1 c2 = foldr f ([], []) c1
    where f = \a (acc1, acc2) -> (a : (map gap c2) ++ acc1, gap a : c2 ++ acc2)

-- intersperse2n m1 m2 returns a new composition with the same tempo and trans
--      as m1 and chord progression of the form
--      [m1.1,m1.2 ... m1.n,m2,m1.(n+1),m1.(n+2), ... m1.2n,m2 ...] 
--      where CN.K is the Kth chord of the Nth composition
--      If (mod m1 n != 0), excess m1 is appended to end of comp without m2
intersperse2n :: Int -> Melody -> Melody -> Composition
intersperse2n n m1 m2 = Composition [newm1, newm2]
  where newm1 = Melody (temp m1) (tran m1) (sperse2na n (chords m1) (chords m2))
        newm2 = Melody (temp m2) (tran m2) (sperse2nb n (chords m1) (chords m2))


-- Helper function for intersperse2n which takes places rests in the first
-- argued composition every n chords, where each rest is the length of the
-- second argued composition
sperse2na :: Int -> [Chord] -> [Chord] -> [Chord]
sperse2na n [] c2 = []
sperse2na n c1 c2 = left ++ (map gap c2) ++ (sperse2na n right c2)
    where left  = List.take n c1
          right = List.drop n c1

-- Helper function for intersperse2n which takes extends the second argued
-- composition to the form [c2,rest,c2,rest...], where each rest is the length 
-- of the next n consecutive chords from the first argued composition
sperse2nb :: Int -> [Chord] -> [Chord] -> [Chord]
sperse2nb n [] c2 = []
sperse2nb n c1 c2 = (map gap (left)) ++ c2 ++ (sperse2nb n right c2)
    where left  = List.take n c1
          right = List.drop n c1 


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
