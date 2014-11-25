Functional Music
EDAN40
Magnus Norrby atn08mn1
Joakim Strand zba10jst

> module AutoComp where
> import Haskore hiding (chord, Key) 




Major scales:

> ionian = [0,2,4,5,7,9,11]
> lydian = [0,2,4,6,7,9,11]
> mixolydian = [0,2,4,5,7,9,10]

Minor scales:

> aeolian = [0,2,3,5,7,8,10]
> dorian = [0,2,3,5,7,9,10]
> phrygian = [0,1,3,5,7,8,10]


Scale mapping:

Byt sen

> major = [0,2,4,5,7,9]
> minor = [0,2,3,5,7,8]

> type Key = (Pitch, [Int])
> type Chord = (PitchClass, Dur)
> type ChordProgression = [Chord]

> pitchToNbr = zip [A,As,B,C,Cs,D,Ds,E,F,Fs,G,A,As,B,C,Cs,D,Ds,E,F,Fs,G] [0,1..]

> getNbrFromPitch :: [(PitchClass, Int)] -> PitchClass -> Int
> getNbrFromPitch (x:xs) p
>    | (fst x) == p = snd x
>    | otherwise = getNbrFromPitch xs p




> type Basstyle = [(Int,Dur)]



> basic, calypso, boogie :: Basstyle
> basic = cycle [(1,hn), (5,hn)]
> calypso = cycle [(-1,qn), (1,en), (3,en), (-1,qn), (1,en), (3,en)]
> boogie = cycle [(1,en), (5,en), (6,en), (5,en), (1,en), (5,en), (6,en), (5,en)]



> wait :: Dur -> ChordProgression -> ChordProgression
> wait _ [] = []
> wait d (c:cp) 
>    | (d-(snd c)) < 0 = (c:cp)
>    | otherwise = wait (d-(snd c)) cp

> getPitch :: Key -> Int -> PitchClass
> getPitch (p,l) i = (fst (pitchToNbr!!(getNbrFromPitch pitchToNbr (fst p)+i))) 


Generating bass line
We use three standard bass patterns (basic, boogie and calypso). Each beat can either be a note or a rest, both with a specific duration. We get the note by looking at the scale of the root chord playing at the moment. The number given is the location in that root notes scale. 

> generateMusic :: PitchClass -> Octave -> Dur -> Music
> generateMusic p o d = Note (p,o) d [Volume 80] 

> autoBass :: Basstyle -> Key -> ChordProgression -> Music
> autobass _ _ [] = []
> autoBass (b:bs) k (c:cp) 
>    | (fst b)== (-1) = Rest (snd b) :+: autoBass bs k (wait (snd b) cp)
>    | otherwise = Instr "bass" (generateMusic (getPitch k ((snd k)!!(fst b))) 3 (snd b)) :+: autoBass bs k (wait (snd b) cp)
  

Our defininition of a chord is in this case the triad. 
A triad is three notes played simultaniously thereby creating a simplyfied version of the full chord.
The first note in the triad is always the root note. The two other notes are on the 3:rd and 5:th position in the keys scale. 
The 3:rd position differs from major to minor while the 5:th position always is 7 steps away from the key. 
 
> getChord :: (Key,Chord) -> Music
> getChord (k,c) =  Instr "guitarr" $ foldr1 (:=:) [ Note ((fst c), 4) (snd c) [Volume 60] | x<-[c, ((getPitch k ((snd k)!!2)),snd(c)), ((getPitch k 5,snd(c)))]]

  
> autoChord :: Key -> ChordProgression -> Music
> autoChord k cp =  foldr1 (:+:) (map getChord (zip(cycle [k]) cp))    