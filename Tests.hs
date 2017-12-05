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
  classify,  maxSuccess, listOf, resize, scale, (==>))

runTests :: IO ()
runTests = undefined

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
    [ "empty" ~: IM.updateWith 1 id IM.empty ~?= Nothing
    , "id" ~: IM.updateWith 1 id (snd $ singleton) ~?= Just (snd $ singleton)
    , "diff" ~: IM.updateWith 1 f (snd $ singleton) ~?= Just (snd $ IM.add IM.empty (Melody 2 2 []))
    ]
        where singleton = IM.add IM.empty (Melody 1 1 [])
              f = (\_ -> Melody 2 2 [])

tGet :: Test
tGet = "IM get" ~: TestList
    [ "empty" ~: IM.get IM.empty 1 ~?= Nothing
    , "single" ~: IM.get (snd $ singleton) 1 ~?= Just (Melody 1 1 [])
    , "mult fst" ~: IM.get mult 1 ~?= Just (Melody 1 1 [])
    , "mult snd" ~: IM.get mult 2 ~?= Just (Melody 2 2 [])
    ]
        where singleton = IM.add IM.empty (Melody 1 1 [])
              mult = snd $ IM.add (snd singleton) (Melody 2 2 [])

----------------------- Parser Unit Tests ---------------------------------
testParser :: Test
testParser = TestList [ testToPitch, testToPrimitive, testToNote, testToChord, testToComposition 

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

-- toMeldoy :: String -> [String] -> Composition
teMeldoy :: Test
teMeldoy = "toMeldoy" ~: TestList
    [ "empty"              ~: toMeldoy "" []                     ~?= mempty
    , "invalid instrument" ~: toMeldoy ""      []                ~?= mempty
    , "invalid note"       ~: toMeldoy "Flute" ["x"]             ~?= m [c2]
    , "invalid rest"       ~: toMeldoy "Flute" ["r,t"]           ~?= m [c2]
    , "single note"        ~: toMeldoy "Flute" ["af,2,1.5"]      ~?= m [c1]
    , "single rest"        ~: toMeldoy "Flute" ["r,3"]           ~?= m [c7]
    , "multiple notes"     ~: toMeldoy "Flute" ["aff,3,3|f,4,4"] ~?= m [c4]
    , "multiple rests"     ~: toMeldoy "Flute" ["r,2|r,3"]       ~?= m [c5]
    , "rests and notes"    ~: toMeldoy "Flute" ["aff,3,3|r,5"]   ~?= m [c6]
    , "multiple chords"    ~: toMeldoy "Flute"
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

----------------------- Unary QuickChecks ---------------------------------
-- setTempo :: Rational -> Composition -> Composition
-- property: setTempo to random Rational, tempo is changed

-- modifyTempo :: Rational -> Composition -> Composition
-- property: add or subtract random Rational, tempo is changed

-- transpose :: Int -> Composition -> Composition
-- property: setTranspose to random Int, trans is changed

-- setInstrument :: InstrumentName -> Composition -> Composition
-- property: setInstrument to random InstrumentName, instr is changed for every Note

-- reverse :: Composition -> Composition
-- property: reverse reverse is same
-- property: reverse is reversed?
-- property: reverse is same length

-- collapse :: Composition -> Composition
-- property: collapse still contains all same notes
-- property: collapse is length 1 afterward (if it was at least length 1 before)

-- take :: Int -> Composition -> Maybe Composition
-- property: take of random Int is length of random Int after (or less)

-- drop :: Int -> Composition -> Maybe Composition
-- property? 

-- splitAt :: Int -> Composition -> (Maybe Composition, Maybe Composition)
-- property?

----------------------- Binary QuickChecks ---------------------------------

-- For composition functions of the form (f c1 c2)

-- Property for all binary functions: tempo and trans for resulting
-- composition is that of c1

-- stack :: Composition -> Composition -> Composition
-- property : len c1 == len c2 => any note in c1 or c2 is in stack c1 c2
-- at same index as in original composition
-- property : the number of times c2 gets copied is = (len c1) mod (len c2)
-- property : length stack c1 c2 = length c1

-- stack2 :: Composition -> Composition -> Composition
-- property : len c1 == len c2 => any note in c1 or c2 is in stack2 c1 c2
-- property : number of occurences of any note in stack c1 c2 = number of
--            occurences in c1 + number of occurences in c2 (if len c1 = len c2)
-- property : all notes that occur at an index i < length c1 & i < length c2 are
--            preserved in stack2 c1 c2
-- property : length of stack c1 c2 = min(length c1, length c2)

-- stack3 :: Composition -> Composition -> Composition
-- property : number of occurences of any note in stack c1 c2 = number of
--            occurences in c1 + number of occurences in c2 (if len c1 = len c2)

-- intersperse1 :: Composition -> Composition -> Composition
-- property : all even indices contain chords from c1
-- property : all odd indices contain chords from c2
-- property : number of occurences of any note, n, in intersperse c1 c2
--            is equal to number of occurences of n1 in c1 + number of 
--            in c2 occurences of n1

-- intersperse2 :: Composition -> Composition -> Composition
-- property : a chord from c1 occurs every index i | i mod (len c2) == 0
-- property : c2 occurs n times in intersperse2 c1 c2, where n = len (c1)

-- intersperse2n :: Composition -> Composition -> Composition
-- property : c2 occurs every n indices in intersperse2n c1 c2.
-- property: if c1 mod n != 0, intersperse2n c1 c2 always ends with excess c1, 
--           not c2

