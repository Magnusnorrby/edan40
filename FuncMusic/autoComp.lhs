Functional Music
EDAN40
Magnus Norrby atn08mn1
Joakim Strand zba10jst

> module AutoComp where
> import Haskore hiding (chord, Key) 
> import Twinkle




Major scales:

> ionian = [0,2,4,5,7,9,11]
> lydian = [0,2,4,6,7,9,11]
> mixolydian = [0,2,4,5,7,9,10]

Minor scales:

> aeolian = [0,2,3,5,7,8,10]
> dorian = [0,2,3,5,7,9,10]
> phrygian = [0,1,3,5,7,8,10]


Scale mapping:

Byt sen Minor chord plats 2 8=9

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
> getPitch (p,l) i = fst (pitchToNbr!!(getNbrFromPitch pitchToNbr (fst p)+i)) 

> getBassPitch :: PitchClass -> Int -> PitchClass
> getBassPitch p i = fst (pitchToNbr!!(getNbrFromPitch pitchToNbr p +i)) 

> checkDorian :: Key -> Int -> PitchClass
> checkDorian (p,l) i
>    | (l == minor && (getNbrFromPitch pitchToNbr (fst p))+2==i) = fst (pitchToNbr!!(getNbrFromPitch pitchToNbr (fst p)+9))
>    | otherwise = fst (pitchToNbr!!(getNbrFromPitch pitchToNbr (fst p)+8))

Generating bass line
We use three standard bass patterns (basic, boogie and calypso). Each beat can either be a note or a rest, both with a specific duration. We get the note by looking at the scale of the root chord playing at the moment. The number given is the location in that root notes scale. 

> generateMusic :: PitchClass -> Octave -> Dur -> Music
> generateMusic p o d = Note (p,o) d [Volume 80] 

> autoBass :: Basstyle -> Key -> ChordProgression -> Music
> autoBass (b:bs) k [(p,d)] = Instr "bass" $ generateMusic (checkDorian k ((snd k)!!(fst b))) 3 (snd b)
> autoBass (b:bs) k ((p,d):cp) 
>    | (fst b)== (-1) = Rest (snd b) :+: autoBass bs k (wait (snd b) ((p,d):cp))
>    | otherwise = Instr "bass" $ generateMusic (checkDorian k ((snd k)!!(fst b))) 3 (snd b) :+: autoBass bs k (wait (snd b) ((p,d):cp))
  

Our defininition of a chord is in this case the triad. 
A triad is three notes played simultaniously thereby creating a simplyfied version of the full chord.
The first note in the triad is always the root note. The two other notes are on the 3:rd and 5:th position in the keys scale. 
The 3:rd position differs from major to minor while the 5:th position always is 7 steps away from the key. 
 
> getChord :: (Chord,Key) -> Music
> getChord (c,k)  = Instr "guitarr" $ foldr1 (:=:) [ Note ((fst x), 4) (snd x) [Volume 60] | x<-[c, ((getPitch k ((snd k)!!2)),(snd c)), ((getPitch k 7),(snd c))]]


  
> autoChord :: Key -> ChordProgression -> Music
> autoChord k cp =  foldr1 (:+:) $ map getChord $ zip cp (cycle [k])  
   

> autoComp :: Basstyle -> Key -> ChordProgression -> Music
> autoComp b k cp =  (autoChord k cp) :=:  (autoBass b k cp)


-- Twinkle Chords

> c1 = [(C,qn), (C,qn), (G,qn), (G,qn), (A,qn), (A,qn), (G,hn)]

> c2 = [(F,qn), (F,qn), (E,qn), (E,qn), (D,qn), (D,qn), (C,hn)]

> c3 = [(G,qn), (G,qn), (F,qn), (F,qn), (E,qn), (E,qn), (D,hn)]

> twinkleLittleStarChords = c1 ++ c2 ++ c3 ++ c3 ++ c1 ++ c2

> testSong = autoComp basic ((C,4) , major) twinkleLittleStarChords;
> twinkleBasic   = twinkleLittleStar :=: autoComp basic ((C,4) , major) twinkleLittleStarChords
> twinkleCalypso = twinkleLittleStar :=: autoComp calypso ((C,4) , major) twinkleLittleStarChords
> twinkleBoogie  = twinkleLittleStar :=: autoComp boogie ((C,4) , major) twinkleLittleStarChords


