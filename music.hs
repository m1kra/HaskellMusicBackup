
import Euterpea

-- part = g 3 hn :+: e 3 hn

-- music = part :+: part :+: g 3 qn :+: a 3 qn :+: g 3 qn :+: f 3 qn :+: e 3 qn :+: f 3 qn

rhand1 = g 3 qn :+: a 3 en :+: g 3 en :+: e 3 qn :+: e 3 qn
lhand1 = let part = c 2 qn :+: g 2 qn in part :+: part
tact1 = lhand1 :=: rhand1

rhand2 = line [e 3 en, d 3 en, e 3 en, f 3 en, e 3 hn]
tact2 = rhand2 :=: lhand1

rhand3 = line [f 2 qn, d 3 en, g 3 en, e 2 hn]
lhand3 = line $ map ($qn) [d 2, g 2, c 2, b 2]
tact3 = rhand3 :=: lhand3

rhand4 = line [c 1 qn, a 1 en, d 1 en, g 1 hn]
lhand4 = line [a 2 qn, d 2 qn, rest qn, g 2 qn]
tact4 = lhand4 :=: rhand4

rhand5 = line [d 1 hn, d 1 en, a 3 en, g 3 en, f 3 en]
lhand5 = line $ map ($qn) [d 2, a 2, d 2, f 3]
tact5 = rhand5 :=: lhand5

music = tact1 :+: tact2 :+: tact3 :+: tact4 :+: tact5