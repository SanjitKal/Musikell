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
    - expand Note (and Chord and Composition) type(s) to store modification data
        - i.e. Duration, Tempo, ??
    

