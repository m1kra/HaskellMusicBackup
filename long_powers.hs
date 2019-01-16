import Euterpea

gen = 2

p = 35

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

mulmod x y = (x * y) `mod` p

powers gen = 
    let pows = 1 : map (mulmod gen)  pows
    in pows

powerNotes gen oct = map (num2note oct) $ powers gen

finalNotes gen oct tm = map ($tm) $ powerNotes gen oct

toPlay gen oct tm = line $ finalNotes gen oct tm
