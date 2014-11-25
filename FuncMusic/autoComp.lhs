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


> major = [ionian, mixolydian, dorian, lydian, mixolydian, dorian, dorian]
> minor = [dorian, dorian, phrygian, dorian, dorian, aeolian, dorian]

> type Key = (Pitch, [[Int]])
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


> findScale :: Key -> PitchClass -> [Int]
> findScale (p1,m) p2
>    | ((getNbrFromPitch pitchToNbr p2) - (getNbrFromPitch pitchToNbr (fst p1))) < 0 =  m!!((getNbrFromPitch pitchToNbr p2) - (getNbrFromPitch pitchToNbr (fst p1)) + 11)
>    | otherwise =  m!!((getNbrFromPitch pitchToNbr p2) - (getNbrFromPitch pitchToNbr (fst p1))) 


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
>    | otherwise = (generateMusic (getPitch k ((findScale k (fst c))!!(fst b))) 4 (snd b)) :+: autoBass bs k (wait (snd b) cp)
     