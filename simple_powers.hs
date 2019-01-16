import Euterpea

gen = 2

p = 7

notes oct = map ($oct) [c, d, e, f, g, a, b]

num2note oct num = notes oct !! num

mulmod x y = (x * y) `mod` p

powers gen = 
    let pows = 1 : map (mulmod gen)  pows
    in pows

powerNotes gen oct = map (num2note oct) $ powers gen

finalNotes gen oct tm = map ($tm) $ powerNotes gen oct

toPlay gen oct tm = line $ finalNotes gen oct tm
