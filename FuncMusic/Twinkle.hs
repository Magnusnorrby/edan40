module Twinkle where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol n = n v
v = [Volume 80]
lmap f l = line (map f l)

-- Twinkle Chords
p1 = lmap vol [c 4 qn, c 4 qn, g 4 qn, g 4 qn, a 4 qn, a 4 qn, g 4 hn]

p2 = lmap vol [f 4 qn, f 4 qn, e 4 qn, e 4 qn, d 4 qn, d 4 qn, c 4 hn]

p3 = lmap vol [g 4 qn, g 4 qn, f 4 qn, f 4 qn, e 4 qn, e 4 qn, d 4 hn]

tm1 = lmap vol [c 4 qn, c 4 qn, g 4 qn, g 4 qn]
tm2 = lmap vol [f 4 qn, f 4 qn, c 4 hn]
tm3 = lmap vol [g 4 qn, g 4 qn, c 4 qn, c 4 qn, g 4 qn, g 4 qn, c 4 hn]
tm4 = lmap vol [c 4 qn, c 4 qn, g 4 hn] 

twinkleMelody = p1 :+: p2 :+: p3 :+: p3 :+: p1 :+: p2

-- Twinkle Chords

c1 = [(C,qn), (C,qn), (G,qn), (G,qn), (A,qn), (A,qn), (G,hn)]

c2 = [(F,qn), (F,qn), (E,qn), (E,qn), (D,qn), (D,qn), (C,hn)]

c3 = [(G,qn), (G,qn), (F,qn), (F,qn), (E,qn), (E,qn), (D,hn)]

tc1 = [(C,qn), (C,qn), (G,qn), (G,qn)]

tc2 = [(F,qn), (F,qn), (C,hn)]

tc3 = [(G,qn), (G,qn), (C,qn), (C,qn), (G,qn), (G,qn), (C,hn)]

tc4 = [(C,qn), (C,qn), (G,hn)]

twinkleLittleStarChords = c1 ++ c2 ++ c3 ++ c3 ++ c1 ++ c2 

--- Finish
twinkleLittleStar = Instr "piano" twinkleMelody



