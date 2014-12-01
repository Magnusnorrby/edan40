module Zelda where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol n = n v
v = [Volume 80]
lmap f l = line (map f l)

-- Zeldas lullaby
z1 = lmap vol [b 4 qn, d 4 qn, a 4 qn]
z2 = lmap vol [g 4 qn, a 4 qn]
z3 = lmap vol [g 4 qn, d 4 qn]
z4 = lmap vol [c 4 qn, b 4 qn, a 4 qn]
zeldaMelody = z1 :+: z2  :+: z1 :+: z1 :+: z3 :+: z4 :+: z1 :+: z2 :+: z1 :+: z1 :+: z3

-- Zeldas lullaby

zc1 = [(B,qn), (D,qn), (A,qn)]
zc2 = [(G,qn), (A,qn)]
zc3 = [(G,qn), (D,qn)]
zc4 = [(D,qn), (B,qn), (A,qn)]

zeldaChords = zc1 ++ zc2  ++ zc1 ++ zc1 ++ zc3 ++ zc4 ++ zc1 ++ zc2 ++ zc1 ++ zc1 ++ zc3

-- Finish

zeldasLullaby = Instr "flute" zeldaMelody