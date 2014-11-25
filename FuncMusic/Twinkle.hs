module Twinkle where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol n = n v
v = [Volume 80]
lmap f l = line (map f l)

-- Twinkle Chords
p1 = lmap vol [g 4 qn, g 4 qn, d 4 qn, d 4 qn, e 4 qn, e 4 qn, d 4 hn]

p2 = lmap vol [c 4 qn, c 4 qn, b 4 qn, b 4 qn, a 4 qn, a 4 qn, g 4 hn]

p3 = lmap vol [d 4 qn, d 4 qn, c 4 qn, c 4 qn, b 4 qn, b 4 qn, a 4 hn]

twinkleChords = p1 :+: p2 :+: p3 :+: p3 :+: p1 :+: p2



-- Finish
twinkleLittleStar = Instr "piano" (Tempo 3 (Phrase [Dyn SF] twinkleChords))