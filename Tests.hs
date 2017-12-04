module Tests where

import Euterpea

import Music
import Parser
import CompositionMap

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))

runTests :: IO ()
runTests = undefined

----------------------- CompositionMap Unit Tests ------------------------

tCompMap = TestList [tEmpty, tAdd, tUpdateWith, tGet]

tEmpty :: Test
tEmpty = "" ~: True ~?= True

tAdd :: Test
tAdd = "exec wFact" ~: True ~?= True

tUpdateWith :: Test
tUpdateWith = "exec wAbs" ~: True ~?= True

tGet :: Test
tGet = "exec wTimes" ~: True ~?= True

----------------------- Parser Unit Tests ---------------------------------
testParser :: Test
testParser = TestList [ testToPitch, testToPrimitive, testToNote, testToChord, testToComposition ]

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

-- toComposition :: String -> [String] -> Composition
testToComposition :: Test
testToComposition = "toComposition" ~: TestList
    [ "empty"              ~: toComposition "" []                     ~?= mempty
    , "invalid instrument" ~: toComposition ""      []                ~?= mempty
    , "invalid note"       ~: toComposition "Flute" ["x"]             ~?= m [c2]
    , "invalid rest"       ~: toComposition "Flute" ["r,t"]           ~?= m [c2]
    , "single note"        ~: toComposition "Flute" ["af,2,1.5"]      ~?= m [c1]
    , "single rest"        ~: toComposition "Flute" ["r,3"]           ~?= m [c7]
    , "multiple notes"     ~: toComposition "Flute" ["aff,3,3|f,4,4"] ~?= m [c4]
    , "multiple rests"     ~: toComposition "Flute" ["r,2|r,3"]       ~?= m [c5]
    , "rests and notes"    ~: toComposition "Flute" ["aff,3,3|r,5"]   ~?= m [c6]
    , "multiple chords"    ~: toComposition "Flute"
        ["f,4,4", "aff,3,3|af,2,1.5", "af,2,1.5", "r,3"]              ~?= m'
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

----------------------- Binary Unit Tests ---------------------------------
