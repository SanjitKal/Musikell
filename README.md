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
