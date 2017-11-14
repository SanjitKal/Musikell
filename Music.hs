type Note = Char

data Sound = Harmony [Note] | Melody [Note]


type Mod = Temp :: Int -> () | Depth :: Int -> ()

applyMod :: Mod -> Sound -> Sound
applyMod (Temp f) (Harmony h) = ...
applyMod (Temp f) (Meldoy m) = different ...


harmony >>= mod
harmony >>= (Temp f) = harmony 

playMelody = ...
instance Playable a of
    play :: a -> (sound)

instance Playable Harmony of
    play (n:ns) = playAll n:ns

add :: Note | Harmony | Melody
remove

setTempo :: 

$ musikell
> m a,b,c 120
> show sounds
{ s1: Melody [a, b, c], 120 }
> t s1 100
> t 1000

melody :: [Note] -> [Mod] -> Melody
harmony :: [Note] -> [Mod] -> Harmony

m -> melody 
n -> note
h -> harmony
t -> tempo (given a sound)
