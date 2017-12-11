module Tests where

import Euterpea

import qualified Data.Map as DM
import Music
import Parser
import qualified CompositionMap as CM
import qualified IncrMap as IM


import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>), Property, quickCheck)

main :: IO ()
main = do _ <- runTestTT tCompMap
          _ <- runTestTT testParser
          return ()
----------------------- CompositionMap Unit Tests ------------------------

tCompMap = TestList [tEmpty, tAdd, tUpdateWith, tGet]

tEmpty :: Test
tEmpty = "IM.empty" ~: (fst IM.empty, null $ snd IM.empty) ~?= (1, True)

tAdd :: Test
tAdd = "IM.add" ~: TestList
    [ "CID single" ~: fst (singleton) ~?= 1
    , "Map single" ~: DM.lookup 1 (snd $ snd $ singleton) ~?= Just (Melody 1 1 [])
    , "incr CID"   ~: fst (IM.add (snd $ singleton) (Melody 2 2 [])) ~?= 2
    , "keep old" ~: DM.lookup 1 newMap ~?= Just (Melody 1 1 [])
    , "store new" ~: DM.lookup 2 newMap ~?= Just (Melody 2 2 [])
    ]
        where singleton = IM.add IM.empty (Melody 1 1 [])
              newMap = snd (snd (IM.add (snd $ singleton) (Melody 2 2 [])))

tUpdateWith :: Test
tUpdateWith = "IM.updateWith" ~: TestList
    [ "empty" ~: IM.updateWith 1 id IM.empty ~?= (Nothing :: Maybe (IM.IncrMap Note))
    , "id" ~: IM.updateWith 1 id (snd $ singleton) ~?= Just (snd $ singleton)
    , "diff" ~: IM.updateWith 1 f (snd $ singleton) ~?= Just (snd $ IM.add IM.empty (Melody 2 2 []))
    ]
        where singleton = IM.add IM.empty (Melody 1 1 [])
              f = (\_ -> Melody 2 2 [])

tGet :: Test
tGet = "IM get" ~: TestList
    [ "empty" ~: IM.get IM.empty 1 ~?= (Nothing :: Maybe (IM.IncrMap Note))
    , "single" ~: IM.get (snd $ singleton) 1 ~?= Just (Melody 1 1 [])
    , "mult fst" ~: IM.get mult 1 ~?= Just (Melody 1 1 [])
    , "mult snd" ~: IM.get mult 2 ~?= Just (Melody 2 2 [])
    ]
        where singleton = IM.add IM.empty (Melody 1 1 [])
              mult = snd $ IM.add (snd singleton) (Melody 2 2 [])

----------------------- Parser Unit Tests ---------------------------------
testParser :: Test
testParser = TestList [ testToPitch, testToPrimitive, testToNote, testToChord]

--Add toComposition test

-- toPitch :: String -> Int -> Pitch
testToPitch :: Test
testToPitch = "toPitch" ~: TestList
    [ "valid"   ~: toPitch "aff"     0 ~?= (Aff, 0)
    , "valid"   ~: toPitch "af"      1 ~?= (Af, 1)
    , "valid"   ~: toPitch "c"       2 ~?= (C, 2)
    , "invalid" ~: toPitch "UNKNOWN" 4 ~?= (C, 4)
    ]

-- toPrimitive :: String -> Primitive Pitch
testToPrimitive :: Test
testToPrimitive = "toPrimitive" ~: TestList
    [ "invalid"      ~: toPrimitive "x"         ~?= Rest 1
    , "invalid rest" ~: toPrimitive "r,"        ~?= Rest 1
    , "valid rest"   ~: toPrimitive "r,3.0"     ~?= Rest 3
    , "invalid note" ~: toPrimitive "af,2"      ~?= Rest 1
    , "invalid note" ~: toPrimitive "x,1,2.0"   ~?= Note 2 (C, 1)
    , "invalid note" ~: toPrimitive "af,2,x"    ~?= Note 1 (Af, 2)
    , "valid note"   ~: toPrimitive "aff,4,3.5" ~?= Note 3.5 (Aff, 4)
    ]

-- toNote :: String -> String -> Note
testToNote :: Test
testToNote = "toNote" ~: TestList
    [ "invalid instrument" ~: toNote "x"         "af,2,1.5" ~?= N (n1, i1)
    , "invalid note"       ~: toNote "Flute"     "x"        ~?= N (r1, i2)
    , "valid"              ~: toNote "Xylophone" "aff,3,3"  ~?= N (n2, i3)
    ] where
        n1 = Note 1.5 (Af, 2)
        i1 = AcousticGrandPiano
        r1 = Rest 1
        i2 = Flute
        n2 = Note 3 (Aff, 3)
        i3 = Xylophone

-- toChord :: String -> String -> Chord
testToChord :: Test
testToChord = "toChord" ~: TestList
    [ "invalid instrument" ~: toChord "x"     "af,2,1.5"      ~?= Chord [N (n1, i1)]
    , "invalid note"       ~: toChord "Flute" "x"             ~?= Chord [N (r 1, i2)]
    , "single note"        ~: toChord "Xylophone" "aff,3,3"   ~?= Chord [N (n2, i3)]
    , "multiple notes"     ~: toChord "Flute" "aff,3,3|f,4,4" ~?= Chord [N (n2, i2), N (n3, i2)]
    , "multiple rests"     ~: toChord "Flute" "r,2|r,3"       ~?= Chord [N (r 2, i2), N (r 3, i2)]
    , "rests and notes"    ~: toChord "Flute" "aff,3,3|r,5"   ~?= Chord [N (n2, i2), N (r 5, i2)]
    ] where
        n1 = Note 1.5 (Af, 2)
        i1 = AcousticGrandPiano
        r  = Rest
        i2 = Flute
        n2 = Note 3 (Aff, 3)
        i3 = Xylophone
        n3 = Note 4 (F, 4)

-- testToMelody :: String -> [String] -> Composition
testToMelody :: Test
testToMelody = "toMeldoy" ~: TestList
    [ "empty"              ~: toMelody "" []                     ~?= mempty
    , "invalid instrument" ~: toMelody ""      []                ~?= mempty
    , "invalid note"       ~: toMelody "Flute" ["x"]             ~?= m [c2]
    , "invalid rest"       ~: toMelody "Flute" ["r,t"]           ~?= m [c2]
    , "single note"        ~: toMelody "Flute" ["af,2,1.5"]      ~?= m [c1]
    , "single rest"        ~: toMelody "Flute" ["r,3"]           ~?= m [c7]
    , "multiple notes"     ~: toMelody "Flute" ["aff,3,3|f,4,4"] ~?= m [c4]
    , "multiple rests"     ~: toMelody "Flute" ["r,2|r,3"]       ~?= m [c5]
    , "rests and notes"    ~: toMelody "Flute" ["aff,3,3|r,5"]   ~?= m [c6]
    , "multiple chords"    ~: toMelody "Flute"
        ["f,4,4", "aff,3,3|af,2,1.5", "af,2,1.5", "r,3"]         ~?= m'
    ] where
        n1 = Note 1.5 (Af, 2)
        i1 = AcousticGrandPiano
        r  = Rest
        i2 = Flute
        n2 = Note 3 (Aff, 3)
        i3 = Xylophone
        n3 = Note 4 (F, 4)
        c1 = Chord [N (n1, i2)]
        c2 = Chord [N (r 1, i2)]
        c3 = Chord [N (n2, i2)]
        c4 = Chord [N (n2, i2), N (n3, i2)]
        c5 = Chord [N (r 2, i2), N (r 3, i2)]
        c6 = Chord [N (n2, i2), N (r 5, i2)]
        c7 = Chord [N (r 3, i2)]
        c8 = Chord [N (n3, i2)]
        c9 = Chord [N (n2, i2), N (n1, i2)]
        m  = Melody 1.5 0
        m' = m [c8, c9, c1, c7]

----------------------------------Quickchecks-----------------------------------

-- Returns true if every element in l2 is a subset of l1
subset :: Eq a => [a] -> [a] -> Bool
subset l1 l2 = all (flip elem $ l1) l2

-- If the notes in the first chord are a subset of the notes in the second
-- chord (and vice versa), then the two chords are 'equivalent', regardless
-- of the ordering of the individual notes in the chord
chord_property :: Chord -> Chord -> Bool
chord_property c1 c2 = let (n1, n2) = (notes c1, notes c2) in
                           if (subset n1 n2) && (subset n2 n1) then c1 == c2
                           else c1 /= c2

--- Composition reversed twice is equal to original composition
reverse_property :: Composition -> Bool
reverse_property c@(Composition ms) = revrev == c
    where revrev = Composition (reverse $ reverse ms)

-- Individual melodies keep their original length when stacked
stack_property :: Melody -> Melody -> Bool
stack_property m1 m2 = let Composition [m1',m2'] = stack m1 m2 in
                           length (chords m1) == length (chords m1') &&
                           length (chords m2) == length (chords m2')

-- When m1 is longer than m2, stackPreserve preserves both original lengths
stack_preserve_property1 :: Melody -> Melody -> Property
stack_preserve_property1 m1 m2 = length (chords m1) >= length (chords m2) ==>
                                 let Composition [m1',m2'] = stackPreserve m1 m2 in
                                 length (chords m1') == length (chords m1) &&
                                 length (chords m2') == length (chords m2)

-- When m1 is shorter than m2, stackPreserve truncates m2 to be m1's length
stack_preserve_property2 :: Melody -> Melody -> Property
stack_preserve_property2 m1 m2 = length (chords m1) < length (chords m2) ==>
                                 let Composition [m1',m2'] = stackPreserve m1 m2 in
                                 length (chords m1') == length (chords m1) &&
                                 length (chords m2') == length (chords m1)

-- The length of m1 is preserved after stack cycling m1 and m2, the length of
-- m2' (the cycled m2) is the same as m1
stack_cycle_property :: Melody -> Melody -> Property
stack_cycle_property m1 m2@(Melody _ _ l) = not (null l) ==>
                             let Composition [m1',m2'] = stackCycle m1 m2 in
                             length (chords m1') == length (chords m1) &&
                             length (chords m2') == length (chords m1)

-- When m1 and m2 are stack truncated to form m1' and m2', the length of m1`
-- and the length of m2' are equal to min(length m1, length m2)
stack_truncate_property :: Melody -> Melody -> Bool
stack_truncate_property m1 m2 = let Composition [m1',m2'] = stackTruncate m1 m2
                                    minl = min (length (chords m1)) (length (chords m2)) in
                                    length (chords m1') == minl &&
                                    length (chords m2') == minl

set_tempo_property :: Rational -> Melody -> Bool
set_tempo_property r m = temp (setTempo r m) == r

modify_tempo_property :: Rational -> Melody -> Bool
modify_tempo_property r m = temp (modifyTempo r m) == r + temp m

transpose_property :: Int -> Melody -> Bool
transpose_property i m = tran (Music.transpose i m) == i

-- This may hang due to stack overflow, so you should specifiy low bounds for 'n'
repl_property :: Int -> Melody -> Property
repl_property n m = n < 100 && n >= 0 ==> length (chords (repl n m)) == length (chords m) * n

repl_negative_property :: Int -> Melody -> Property
repl_negative_property n m = n <= 0 ==> chords (repl n m) == []

collapse_melody_property :: Melody -> Property
collapse_melody_property m = not (null (chords m)) ==>
                             let m' = collapseMelody m in
                             temp m' == temp m && tran m' == tran m &&
                             length (chords m') == 1 &&
                             containsAll m' m where
    containsAll m1 m2 = all (subset ((notes $ head $ chords m1)) . notes) (chords m2)


take_property :: Int -> Melody -> Property
take_property n m = n > 0 && n < length (chords m) ==> length (chords $ Music.take n m) == n

drop_property :: Int -> Melody -> Property
drop_property n m = n > 0 && n < (length (chords m)) && n < length (chords m) ==>
                    length (chords $ Music.drop n m) == (length (chords m)) - n

split_property :: Int -> Melody -> Property
split_property n m = n > 0 && n < (length $ chords m) ==>
                     let (Just m1, Just m2) = Music.splitAt n m in
                         length (chords m1) == n && length (chords m2) == length (chords m) - n


