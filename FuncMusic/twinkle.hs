module Twinkle where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol n = n v
v = [Volume 80]
lmap f l = line (map f l)

-- Twinkle Melody
p1 = lmap vol [g 4 qn, g 4 qn, d 4 qn, d 4 qn, e 4 qn, e 4 qn, d 4 hn]

p2 = lmap vol [c 4 qn, c 4 qn, b 4 qn, b 4 qn, a 4 qn, a 4 qn, g 4 hn]

p3 = lmap vol [d 4 qn, d 4 qn, c 4 qn, c 4 qn, b 4 qn, b 4 qn, a 4 hn]

twinkleMelody = p1 :+: p2 :+: p3 :+: p3 :+: p1 :+: p2

-- Zeldas lullaby
z1 = lmap vol [B 4 qn, D 4 qn, A 4 qn]
z2 = lmap vol [G 4 qn, A 4 qn]
z3 = lmap vol [B 4 qn, D 4 qn]
z4 = lmap vol [A 5 qn, G 5 qn, D 4 en, C 4 en, B 4 en, A 4 qn]
zeldaMelody = z1 :+: z2 :+: z1 :+: z3 :+: z4

B D A 
G A 
B D A 
B D ((en oktav upp) A G ) D C B A

-- Finish
twinkleLittleStar = Instr "piano" (Tempo 3 (Phrase [Dyn SF] twinkleMelody))

zeldasLullaby = Instr "flute" (Tempo 3 (Phrase [Dyn SF] zeldaMelody))