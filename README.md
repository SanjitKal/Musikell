# Musikell
Welcome to Musikell!
----------------------------------------------------
Authors: Matthew Chiaravalloti and Sanjit Kalapatapu
----------------------------------------------------
Musikell is an interactive command line interface that allows you to create
and compose different sounds, chords, and melodies! To run Musikell, type the
following commands into ghci:

> :l Main.hs
> main

You have now entered our command line interface. Let's create the following
piano melody using command below:

> melodize p e,4,0.5 e,4,0.5 f,4,0.5 g,4,0.5 g,4,0.5 f,4,0.5 e,4,0.5 d,4,0.5 c,4,0.5 c,4,0.5 d,4,0.5 e,4,0.5 e,4,0.75 d,4,0.25 d,4,0.25

Now, we can set the tempo of our melody by calling our setTempo function on our
new melody ID.

> setTempo 2.0 (respective melody id)

Finally, let's play our modified melody!

> play m (respective melody id)

Wow! That sounds familiar... However, we aren't using state of the art, 20th
century software to create boring 17th century music. Let's try something a
little more interesting...

Create a percussion beat with the following command.

> melodize Percussion fs,2,1.0|b,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0|d,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0 r,1.0 fs,2,1.0 fs,2,1.0|b,2,1.0 fs,2,1.0 fs,2,1.0|d,2,1.0 fs,2,1.0 fs,2,1.0 fs,2,1.0

Loop the beat 8 times.

> repl 8 (respective melody id)

Create a rest with a duration of 128 beats.

> melodize Percussion r,128.0

Stick the rest in front of the percussion beat.

seq (rest melody id) (percussion melody id)

Set the tempo of this combined melody to 12.8.

> setTempo 12.8 (combined melody id)

You can listen to the percussion with the following command:

> play m (combined melody id)

Now that we have the percussion, lets create a piano melody.

> melodize p e,5,1.0 e,5,1.0 e,5,1.0 e,4,1.0 ef,5,1.0 ef,5,1.0 ef,5,1.0 ef,4,1.0 df,5,1.0 df,5,1.0 df,5,1.0 df,4,1.0 a,4,1.0 a,4,1.0 af,4,1.0 e,5,1.0

Loop the piano melody 2 times.

> repl 2 (piano melody id)

Set the tempo of the piano melody to 1.6.

> setTempo 1.6 (piano melody id)

You can listen to the piano melody with the following command:
 
> play m (piano melody id)

Now lets combine the percussion and piano (make sure to use the final percussion
and piano melody ids)

> compose (percussion melody id) (piano melody id)

Play the resulting composition with the following command:

> play c (composition id)

To the initiated ear, this sounds like "Runaway" by Kanye West. That's what we're
talking about!
--------------------------------------------------------------------------------
Here is the melody and percussion for Simple Man by Lynyrd Skynard!

Simple Man guitar:
> melodize AcousticGuitarNylon a,3,0.0625 b,3,0.0625

> melodize AcousticGuitarNylon c,3,0.0625 g,3,0.0625 e,3,0.0625 c,3,0.0625 e,5,0.0625 g,3,0.0625 e,3,0.0625 g,3,0.0625 g,2,0.0625 d,3,0.0625 b,3,0.0625 g,2,0.0625 g,3,0.0625 d,3,0.0625 b,3,0.0625 d,3,0.0625 a,3,0.0625 a,4,0.0625 e,3,0.0625 a,3,0.0625 c,4,0.0625 a,4,0.0625 e,3,0.0625 a,4,0.0625 a,3,0.0625 c,4,0.0625 a,4,0.0625 e,3,0.0625 g,4,0.0625 c,4,0.0625 a,3,0.0625 b,3,0.0625

> repl 2 (second guitar id)

> seq (first melody id) (second melody id)

> setTempo 0.5 (combined melody id)

> melodize Percussion r,2.0 ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625|as,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 c,2,0.0625 ess,2,0.0625 c,2,0.0625 ess,2,0.0625 c,2,0.0625 css,2,0.0625 ess,2,0.0625 ess,2,0.0625 ess,2,0.0625

> setTempo 0.5 (percussion melody id)

> compose (combined guitar melody id) (percussion melody id)
