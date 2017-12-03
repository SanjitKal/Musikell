module Parser where

import Euterpea
import Music
import Text.Read (readMaybe)
import Data.Text (splitOn, pack, unpack)
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

split :: String -> String -> [String]
split d s = map unpack (splitOn (pack d) (pack s))

toComposition :: [String] -> Composition
toComposition = Melody . map toChord

toChord :: String -> Chord
toChord = Chord . map toPrimitive . split "|"

toPitch :: String -> Int -> Pitch
toPitch n o = (toPitchClass n, o :: Octave) :: Pitch

toPrimitive :: String -> Primitive Pitch
toPrimitive s = case (split "," s) of
                     ["r", dur]     -> case readMaybe dur :: Maybe Rational of
                                            Just d -> Rest d
                                            Nothing -> Rest 1
                     [pc, dur, oct] -> case (readMaybe dur :: Maybe Rational, readMaybe oct :: Maybe Int) of
                                            (Just d, Just o) -> Note d $ toPitch pc o
                                            (Just d, Nothing) -> Note d $ toPitch pc 4
                                            (Nothing, Just o) -> Note 1 $ toPitch pc o
                                            (Nothing, Nothing) -> Note 1 $ toPitch pc 4
                     _              -> Rest 1

toPitchClass :: String -> PitchClass
toPitchClass "cff" =  Cff
toPitchClass "cf"  =  Cf
toPitchClass "c"   =  C
toPitchClass "cs"  =  Cs
toPitchClass "css" =  Css
toPitchClass "dff" =  Dff
toPitchClass "df"  =  Df
toPitchClass "d"   =  D
toPitchClass "ds"  =  Ds
toPitchClass "dss" =  Dss
toPitchClass "eff" =  Eff
toPitchClass "ef"  =  Ef
toPitchClass "e"   =  E
toPitchClass "es"  =  Es
toPitchClass "ess" =  Ess
toPitchClass "fff" =  Fff
toPitchClass "ff"  =  Ff
toPitchClass "f"   =  F
toPitchClass "fs"  =  Fs
toPitchClass "fss" =  Fss
toPitchClass "gff" =  Gff
toPitchClass "gf"  =  Gf
toPitchClass "g"   =  G
toPitchClass "gs"  =  Gs
toPitchClass "gss" =  Gss
toPitchClass "aff" =  Aff
toPitchClass "af"  =  Af
toPitchClass "a"   =  A
toPitchClass "as"  =  As
toPitchClass "ass" =  Ass
toPitchClass "bff" =  Bff
toPitchClass "bf"  =  Bf
toPitchClass "b"   =  B
toPitchClass "bs"  =  Bs
toPitchClass "bss" =  Bss
toPitchClass _     =  C