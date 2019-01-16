import Euterpea

base = 35

notes oct = map ($oct) [c, d, e, f, g, a, b]

longNotes oct = map ($oct) 
    [cff, cf, c, cs, css,
     dff, df, d, ds, dss,
     eff, ef, e, es, ess,
     fff, ff, f, fs, fss,
     gff, gf, g, gs, gss,
     aff, af, a, as, ass,
     bff, bf, b, bs, bss] 

num2note oct num = longNotes oct !! num

mulmod gen x y = ((x * y) `mod` gen) `mod` base

powers p q = 
    let pows = 1 : map (mulmod p q)  pows
    in pows

tmpNotes  p q oct = map (num2note oct) $ powers p q

finalNotes p q oct tm = line $ map ($tm) $ tmpNotes p q oct

toPlay p q oct1 tm1 oct2 tm2 = finalNotes p q oct1 tm1 :=: finalNotes q p oct2 tm2

-- toPlay 2 26 2 hn 3 qn
-- toPlay 13 30 4 qn 3 qn