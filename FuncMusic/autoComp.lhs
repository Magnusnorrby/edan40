Functional Music
EDAN40
Magnus Norrby atn08mn1
Joakim Strand zba10jst
This program is a guide on how to autogenerate comp instruments for a melody.
We will start by describing the basics of the needed music theory and then continue
to describe our implementation of the functions. The solution is built around Haskore
which is a library containing needed defenitions such as Pitch, Note and Music.

> module AutoComp where
> import Haskore hiding (chord, Key)
> import Twinkle
> import Zelda

--- Basic music theory
We start out by defining our seven notes:
A B C D E F G
Where A usually is defined as 440 hz. These notes are defined in an infinite cycle
where every cycle is called an octave. This means that if we go from A through B , C
and so on and finish on the next A we have moved one octave up. The distance between
our seven notes is not equal. This is easiest seen on a piano where the full notes are
the white keys and the step between is the black notes. For example between the notes A and B
there is a black key. This note has two names, A sharp and B flat. Sharp means one half step above and flat means one half step below. Looking at a piano we see that there is a black key between all notes except between B-C and E-F. This means that B sharp for example is C. This gives us a total
of 12 steps for each octave.

--- Scales
We then come to the theory of scales. Some notes or combination of notes (chords) sound good together and some does not fit that well to the human ear. If we for example decide that we want to play a song in the key of C there are only certain chords that will fit in to that composition. The order of the notes or chords doesn't matter though.
The definition of which notes goes with which keys are usually called scales. A scale always starts
with it's own note. For example the C Major scale starts with a C. The name Major scale means that
it is used to form major chords and Minor scale means that it is used to form minor chords.
As can be seen from the 6 scales below the Minor scales sometimes take shorter steps then
the Major scales and therefore gives a darker more gloomy sound.
Major scales:

> ionian = [0,2,4,5,7,9,11]
> lydian = [0,2,4,6,7,9,11]
> mixolydian = [0,2,4,5,7,9,10]

Minor scales:

> aeolian = [0,2,3,5,7,8,10]
> dorian = [0,2,3,5,7,9,10]
> phrygian = [0,1,3,5,7,8,10]

--- Creating a chord
Since we want to create music to accompanate a melody we want to create chords that fit
with a note. We will be creating a short version of a full chord called a triad. Here we take
the root note and the third and fifth note of the corresponding scale. The scale to be chosen
depends on the Key we play in and the root note of our chord. As can be seen in the above scales
we see that the thrid and fifth position is the same for all major and minor scales but differ
between the two groups. We take advantage of this and create two theoretically wrong but
simplified scales called major and minor. These scales allows us to correctly create triads
without calculating which scale we actually need.

> major = [0,2,4,5,7,9]
> minor = [0,2,3,5,7,8]

--- Definitions
To be able to implement the program we need some basic definitions. We start out by defining
Key as a Pitch and a scale (major or minor). We then define our chord as a PitchClass and a duration.
Where a duration in our example can be either of the three: half note, quarter note and eight note.
We also define a chord progression as a list of chords.

> type Key = (Pitch, [Int])
> type Chord = (PitchClass, Dur)
> type ChordProgression = [Chord]

--- Basstyles
We also want bass to accompanate our melody. The bass progression consists of single notes
in different predefined patterns. Each pattern is a list containing the position in the chosen
scale and its duration. We also have the option to have a rest isntead of a note which we denote
with index -1. Below we have three example bass patterns called basic, calypso and boogie.
As can be seen in the patterns below we also run into a problem with our simplification of
the major and minor scales. The major scale simplification is still satisfactory but the minor
scale fails at the sixth position when the scale is Dorian. We see that both Aeolian and Phrygian
gives a 9 at the sixth position while the Dorian gives an 8. Since its only one exception we
have chosen to add a check for this special case in our functions. If other bass patterns
where to be added it might be better to go with a more theoretically correct solution.

> type Basstyle = [(Int,Dur)]
> basic, calypso, boogie :: Basstyle
> basic = cycle [(1,hn), (5,hn)]
> calypso = cycle [(-1,qn), (1,en), (3,en), (-1,qn), (1,en), (3,en)]
> boogie = cycle [(1,en), (5,en), (6,en), (5,en), (1,en), (5,en), (6,en), (5,en)]

Since we will be working with scales for a generic note we will be using the distance between notes
as an integer. We therefore need a mapping between a PitchClass and integers to succesfully
move between them. As seen below we chose to call the half steps sharp and not flat even though
it would be equivalent.

> pitchToNbr = zip [A,As,B,C,Cs,D,Ds,E,F,Fs,G,A,As,B,C,Cs,D,Ds,E,F,Fs,G] [0,1..]

--- Helper functions
We then create a helper function that takes the list pitchToNbr and a pitch and returns the corresponding integer.

> getNbrFromPitch :: [(PitchClass, Int)] -> PitchClass -> Int
> getNbrFromPitch (x:xs) p
>   | (fst x) == p = snd x
>   | otherwise = getNbrFromPitch xs p

When traversing a chord progression we will also run into the problem that the bass note and the chord have different
durations. We have solved this by adding a function Wait. This function takes the duration of a bass note and
the chord progression and makes sure that they are syncronized.

> wait :: Dur -> ChordProgression -> ChordProgression
> wait _ [] = []
> wait d ((c,cd):cp)
>   | d == cd = cp
>   | d > cd = wait (d-cd) cp
>   | otherwise = ((c,cd-d):cp)


As described above we also need our Dorian exception for bass generation. This function checks if we are playing in a minor scale
and if thats the case checks if we are using the sixth position. In that case we need to check if the real scale used is Dorian.
This is determined by the distance between our Key and the chord playing. If that distance is two we are using the Dorian
scale and the returned PitchClass should be raised one half step.

> checkDorian :: Key -> Int -> Int -> PitchClass -> PitchClass 
> checkDorian (p,l) ib d c
>   | (l == minor && (getNbrFromPitch pitchToNbr (fst p))+2==d) = fst (pitchToNbr!!((getNbrFromPitch pitchToNbr (fst p))+(dorian!!(ib-1)))) 
>   | otherwise = (fst (pitchToNbr!!(mod ((getNbrFromPitch pitchToNbr c)+(l!!(ib-1))) 12))) 

The function getChord uses the definition of a triad discussed above. It takes a chord and the Key to calculate the three notes to
compose the triad. It then takes the triad and creates a Haskore music object with the three notes played simultaneously.

> getChord :: (Chord,Key) -> Music
> getChord ((p,d),k) = Instr "guitarr" $ foldr1 (:=:) [ Note ((fst x), 4) (snd x) [Volume 60] | x<-[(p,d), (fst (pitchToNbr!!(mod (getNbrFromPitch pitchToNbr p + 4) 12)),d), (fst (pitchToNbr!!(mod (getNbrFromPitch pitchToNbr p + 7) 12)),d)]]

The last helper function is generateMusic. It takes a supplied PitchClass, Octave and duration and creates a Haskore music object.

> generateMusic :: PitchClass -> Octave -> Dur -> Music
> generateMusic p o d = Note (p,o) d [Volume 80]

--- Auto comp functions
Atlast we have all the functions to create accompaning music to a melody.
We have the function autoBass which takes a predefined bass pattern, the Key of our song and a chord progression. It then returns
the corresponding bass line.

> autoBass :: Basstyle -> Key -> ChordProgression -> Music
> autoBass _ _ [] = Instr "bass" $ Note (A,1) en [Volume 0]
> autoBass ((ib,idr):bs) k ((p,d):cp)
>   | ib==(-1) = Rest idr :+: autoBass bs k (wait idr ((p,d):cp))
>   | otherwise = Instr "bass" $ generateMusic (checkDorian k ib ((snd k)!!ib) p) 3 idr :+: autoBass bs k (wait idr ((p,d):cp))

AutoChord takes the Key and the ChordProgression of the melody and creates a triad for each note.

> autoChord :: Key -> ChordProgression -> Music
> autoChord k cp = foldr1 (:+:) $ map getChord $ zip cp (cycle [k])

AutoComp combines the two fucntions above and return both the bass line and the guitar comp as a combined music object.

> autoComp :: Basstyle -> Key -> ChordProgression -> Music
> autoComp b k cp = (autoChord k cp) :=: (autoBass b k cp)
> twinkleBasic = twinkleLittleStar :=: (autoComp basic ((C,4) , major) twinkleLittleStarChords)
> twinkleCalypso = twinkleLittleStar :=: (autoComp calypso ((C,4) , major) twinkleLittleStarChords)
> twinkleBoogie = twinkleLittleStar :=: (autoComp boogie ((C,4) , major) twinkleLittleStarChords)
> zeldaBasic = zeldasLullaby :=: (autoComp basic ((C,4) , major) zeldaChords)


