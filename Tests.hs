module Tests where

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

----------------------- Binary Unit Tests ---------------------------------