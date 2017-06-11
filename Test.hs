module Test where

import Euterpea

mel1 :: Music Pitch
mel1 = (Prim (Note (1 / 4) (As,5)) :+: ((Prim (Note (1 / 8) (B,4)) :=: (Prim (Note (1 / 8) (C,4)) :=: Prim (Rest (0 / 1)))) :+: (Prim (Rest (1 / 4)) :+: (Prim (Note (1 / 4) (D,3)) :+: Prim (Rest (0 / 1)))))) :=: Prim (Rest (0 /
 1))
