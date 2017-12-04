module Parser where

import Euterpea
import Music
import Text.Read (readMaybe)
import Data.Text (splitOn, pack, unpack)
import Test.HUnit (runTestTT, Test(..), Assertion, (~?=), (~:), assert)

split :: String -> String -> [String]
split d s = map unpack (splitOn (pack d) (pack s))

toComposition :: String -> [String] -> Composition
toComposition i = Melody 1.5 0 . map (toChord i)

toChord :: String -> String -> Chord
toChord i = Chord . map (toNote i) . split "|"

toNote :: String -> String -> Note
toNote i n = N (toPrimitive n, toInstrumentName i)

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

toPitch :: String -> Int -> Pitch
toPitch n o = (toPitchClass n, o :: Octave) :: Pitch

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

toInstrumentName :: String -> InstrumentName
toInstrumentName "AcousticGrandPiano"  = AcousticGrandPiano
toInstrumentName "BrightAcousticPiano" = BrightAcousticPiano  
toInstrumentName "ElectricGrandPiano"  = ElectricGrandPiano
toInstrumentName "HonkyTonkPiano"      = HonkyTonkPiano
toInstrumentName "RhodesPiano"         = RhodesPiano  
toInstrumentName "ChorusedPiano"       = ChorusedPiano
toInstrumentName "Harpsichord" = Harpsichord  
toInstrumentName "Clavinet" = Clavinet
toInstrumentName "Celesta" = Celesta  
toInstrumentName "Glockenspiel" = Glockenspiel
toInstrumentName "MusicBox" = MusicBox
toInstrumentName "Vibraphone" = Vibraphone
toInstrumentName "Marimba" = Marimba  
toInstrumentName "Xylophone" = Xylophone
toInstrumentName "TubularBells" = TubularBells
toInstrumentName "Dulcimer" = Dulcimer
toInstrumentName "HammondOrgan" = HammondOrgan
toInstrumentName "PercussiveOrgan" = PercussiveOrgan  
toInstrumentName "RockOrgan" = RockOrgan
toInstrumentName "ChurchOrgan" = ChurchOrgan  
toInstrumentName "ReedOrgan" = ReedOrgan
toInstrumentName "Accordion" = Accordion
toInstrumentName "Harmonica" = Harmonica
toInstrumentName "TangoAccordion" = TangoAccordion
toInstrumentName "AcousticGuitarNylon" = AcousticGuitarNylon  
toInstrumentName "AcousticGuitarSteel" = AcousticGuitarSteel  
toInstrumentName "ElectricGuitarJazz" = ElectricGuitarJazz
toInstrumentName "ElectricGuitarClean" = ElectricGuitarClean  
toInstrumentName "ElectricGuitarMuted" = ElectricGuitarMuted  
toInstrumentName "OverdrivenGuitar" = OverdrivenGuitar
toInstrumentName "DistortionGuitar" = DistortionGuitar
toInstrumentName "GuitarHarmonics" = GuitarHarmonics  
toInstrumentName "AcousticBass" = AcousticBass
toInstrumentName "ElectricBassFingered" = ElectricBassFingered
toInstrumentName "ElectricBassPicked" = ElectricBassPicked
toInstrumentName "FretlessBass" = FretlessBass
toInstrumentName "SlapBass1" = SlapBass1
toInstrumentName "SlapBass2" = SlapBass2
toInstrumentName "SynthBass1" = SynthBass1
toInstrumentName "SynthBass2" = SynthBass2
toInstrumentName "Violin" = Violin
toInstrumentName "Viola" = Viola
toInstrumentName "Cello" = Cello
toInstrumentName "Contrabass" = Contrabass
toInstrumentName "TremoloStrings" = TremoloStrings
toInstrumentName "PizzicatoStrings" = PizzicatoStrings
toInstrumentName "OrchestralHarp" = OrchestralHarp
toInstrumentName "Timpani" = Timpani  
toInstrumentName "StringEnsemble1" = StringEnsemble1  
toInstrumentName "StringEnsemble2" = StringEnsemble2  
toInstrumentName "SynthStrings1" = SynthStrings1
toInstrumentName "SynthStrings2" = SynthStrings2
toInstrumentName "ChoirAahs" = ChoirAahs
toInstrumentName "VoiceOohs" = VoiceOohs
toInstrumentName "SynthVoice" = SynthVoice
toInstrumentName "OrchestraHit" = OrchestraHit
toInstrumentName "Trumpet" = Trumpet  
toInstrumentName "Trombone" = Trombone
toInstrumentName "Tuba" = Tuba
toInstrumentName "MutedTrumpet" = MutedTrumpet
toInstrumentName "FrenchHorn" = FrenchHorn
toInstrumentName "BrassSection" = BrassSection
toInstrumentName "SynthBrass1" = SynthBrass1  
toInstrumentName "SynthBrass2" = SynthBrass2  
toInstrumentName "SopranoSax" = SopranoSax
toInstrumentName "AltoSax" = AltoSax  
toInstrumentName "TenorSax" = TenorSax
toInstrumentName "BaritoneSax" = BaritoneSax  
toInstrumentName "Oboe" = Oboe
toInstrumentName "Bassoon" = Bassoon  
toInstrumentName "EnglishHorn" = EnglishHorn  
toInstrumentName "Clarinet" = Clarinet
toInstrumentName "Piccolo" = Piccolo  
toInstrumentName "Flute" = Flute
toInstrumentName "Recorder" = Recorder
toInstrumentName "PanFlute" = PanFlute
toInstrumentName "BlownBottle" = BlownBottle  
toInstrumentName "Shakuhachi" = Shakuhachi
toInstrumentName "Whistle" = Whistle  
toInstrumentName "Ocarina" = Ocarina  
toInstrumentName "Lead1Square" = Lead1Square  
toInstrumentName "Lead2Sawtooth" = Lead2Sawtooth
toInstrumentName "Lead3Calliope" = Lead3Calliope
toInstrumentName "Lead4Chiff" = Lead4Chiff
toInstrumentName "Lead5Charang" = Lead5Charang
toInstrumentName "Lead6Voice" = Lead6Voice
toInstrumentName "Lead7Fifths" = Lead7Fifths  
toInstrumentName "Lead8BassLead" = Lead8BassLead
toInstrumentName "Pad1NewAge" = Pad1NewAge
toInstrumentName "Pad2Warm" = Pad2Warm
toInstrumentName "Pad3Polysynth" = Pad3Polysynth
toInstrumentName "Pad4Choir" = Pad4Choir
toInstrumentName "Pad5Bowed" = Pad5Bowed
toInstrumentName "Pad6Metallic" = Pad6Metallic
toInstrumentName "Pad7Halo" = Pad7Halo
toInstrumentName "Pad8Sweep" = Pad8Sweep
toInstrumentName "FX1Train" = FX1Train
toInstrumentName "FX2Soundtrack" = FX2Soundtrack
toInstrumentName "FX3Crystal" = FX3Crystal
toInstrumentName "FX4Atmosphere" = FX4Atmosphere
toInstrumentName "FX5Brightness" = FX5Brightness
toInstrumentName "FX6Goblins" = FX6Goblins
toInstrumentName "FX7Echoes" = FX7Echoes
toInstrumentName "FX8SciFi" = FX8SciFi
toInstrumentName "Sitar" = Sitar
toInstrumentName "Banjo" = Banjo
toInstrumentName "Shamisen" = Shamisen
toInstrumentName "Koto" = Koto
toInstrumentName "Kalimba" = Kalimba  
toInstrumentName "Bagpipe" = Bagpipe  
toInstrumentName "Fiddle" = Fiddle
toInstrumentName "Shanai" = Shanai
toInstrumentName "TinkleBell" = TinkleBell
toInstrumentName "Agogo" = Agogo
toInstrumentName "SteelDrums" = SteelDrums
toInstrumentName "Woodblock" = Woodblock
toInstrumentName "TaikoDrum" = TaikoDrum
toInstrumentName "MelodicDrum" = MelodicDrum  
toInstrumentName "SynthDrum" = SynthDrum
toInstrumentName "ReverseCymbal" = ReverseCymbal
toInstrumentName "GuitarFretNoise" = GuitarFretNoise  
toInstrumentName "BreathNoise" = BreathNoise  
toInstrumentName "Seashore" = Seashore
toInstrumentName "BirdTweet" = BirdTweet
toInstrumentName "TelephoneRing" = TelephoneRing
toInstrumentName "Helicopter" = Helicopter
toInstrumentName "Applause" = Applause
toInstrumentName "Gunshot" = Gunshot  
toInstrumentName "Percussion" = Percussion
toInstrumentName _ = AcousticGrandPiano
