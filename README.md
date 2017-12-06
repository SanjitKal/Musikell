# Musikell

Progress so far: We have defined types for notes, chords, melodies, and modifications. We have defined and implemented some state
manipulation functions (add, remove, applyMod, etc.).

What needs to be done: We need to figure out how to output actual sounds/how sounds are actually represented. Regarding the scope
of our project: will there be enough depth if we just focus on the composition aspect (excluding the signal aspect)?


-- harmony or melody paramterized by foldable a


-- harmony quickcheck: order of notes doesn't matter!
-- melody quickcheck: order does matter (think about representations of compositions; is there a way to create library s.t. the data level (==) matches what we considers equivalence to our ears)

-- Types we'll need to use from Euterpea
-- Primitive, Music a; Music Pitch
-- Pitch is a tuple of Picthclass and Octave; so we'll have Music a ~ Music Pitch

-- haskell project structure: cabal project (like package.json or Gemfile, but for haskell)


-- 12/2: Notes for next time:
    - unit test what we currently have
    - come up with the "cool modifications"
        - write the type signatures
        - add quickchecks for these
        - implement these
    - tempo is a function over a Composition (map Dur multiplier over each note in the chord?)
    - Combining Compositions
        - append C2 to C1
        - prepend C2 to C1
        - interleave C2 into C1
            - (chord-by-chord?)
            - whole melody C2 between each chord C1
        - Stack
            - instruments would be helpful (Modify)
            - i.e. put drum beat behind each chord or whatever
    - Manipulating existing Compositions
        - intersperse
        - reverse
        - (list functions)
        - tempo


- Matthew: Unary (implement, parser, quickCheck),
            Unit test: toComposition (and dependents)
- Sanjit: Binary (implement, parser, quickCheck),
            Unit test: split and CompositionMap


another one bites the dust bass: compose ElectricBassFingered a,2,0.0625 g,2,0.0625 e,2,0.1875 r,0.125 e,2,0.1875 r,0.125 e,2,0.1875 r,0.125 d,2,0.0625 r,0.125 e,2,0.0625 e,2,0.0625 e,2,0.0625 g,2,0.125 r,0.0625 e,2,0.0625 a,2,0.0625 r,0.25 r,0.125

another one bites the dust drums(kinda): compose Percussion b,3,0.0625 r,0.0625 b,2,0.0625

Ode to (lame) joy:
melodize p e,4,0.5 e,4,0.5 f,4,0.5 g,4,0.5 g,4,0.5 f,4,0.5 e,4,0.5 d,4,0.5 c,4,0.5 c,4,0.5 d,4,0.5 e,4,0.5 e,4,0.75 d,4,0.25 d,4,0.25
setTempo 2.0 MID
play m MID

Runaway drumbeat:
melodize Percussion fs,2,1.0|b,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0|d,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0 r,1.0 fs,2,1.0 fs,2,1.0|b,2,1.0 fs,2,1.0 fs,2,1.0|d,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0

repl 8 MID1

melodize Percussion r,128.0
seq MID2 MID1

setTempo 12.8 MID3

Runaway piano:
melodize p e,5,1.0 e,5,1.0 e,5,1.0 e,4,1.0 ef,5,1.0 ef,5,1.0 ef,5,1.0 ef,4,1.0 df,5,1.0 df,5,1.0 df,5,1.0 df,4,1.0 a,4,1.0 a,4,1.0 af,4,1.0 e,5,1.0

repl 2 MID4

setTempo 1.6 MID4

compose MID4 MID3

Simple Man guitar:
melodize AcousticGuitarNylon a,3,0.0625 b,3,0.0625

melodize AcousticGuitarNylon c,3,0.0625 g,3,0.0625 e,3,0.0625 c,3,0.0625 e,5,0.0625 g,3,0.0625 e,3,0.0625 g,3,0.0625 g,2,0.0625 d,3,0.0625 b,3,0.0625 g,2,0.0625 g,3,0.0625 d,3,0.0625 b,3,0.0625 d,3,0.0625 a,3,0.0625 a,4,0.0625 e,3,0.0625 a,3,0.0625 c,4,0.0625 a,4,0.0625 e,3,0.0625 a,4,0.0625 a,3,0.0625 c,4,0.0625 a,4,0.0625 e,3,0.0625 g,4,0.0625 c,4,0.0625 a,3,0.0625 b,3,0.0625

repl 2 MID2

seq MID1 MID2

setTempo 0.5 MID3

Simple Man drumbeat:

melodize Percussion r,2.0 ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 ess,2,0.0625 c,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625

setTempo 0.5 MID4

compose MID3 MID4

DO NOT PLAY ANYTHING BELOW THIS LINE
-----------------------------

c = kick
as = crash cymbal
css = snare
ess = closed hi-hat

melodize AcousticGuitarNylon 
a,4,0.0625 b,4,0.0625
c,4,0.0625 g,4,0.0625 e,4,0.0625 c,4,0.0625
e,5,0.0625 g,4,0.0625 e,4,0.0625 g,4,0.0625
g,3,0.0625 d,4,0.0625 b,4,0.0625 g,3,0.0625
g,4,0.0625 d,4,0.0625 b,4,0.0625 d,4,0.0625 
a,4,0.0625 a,5,0.0625 e,4,0.0625 a,4,0.0625 
c,5,0.0625 a,5,0.0625 e,4,0.0625 a,5,0.0625 
a,4,0.0625 c,5,0.0625 a,5,0.0625 e,4,0.0625 
g,5,0.0625 c,5,0.0625 a,4,0.0625 b,4,0.0625

melodize Percussion 
ess,2,0.0625 ess,2,0.0625
c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625
c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625
ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625
ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625
ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625
ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625
ess,2,0.0625 ess,2,0.0625 c,2,0.0625 ess,2,0.0625
c,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625


Runaway:
compose MID1 MID2

play c CID
