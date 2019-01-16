import Euterpea

base = 7

notes oct = map ($oct) [c, d, e, f, g, a, b]

num2note oct num = notes oct !! num

mulmod x y = (x * y) `mod` base

powers gen = 
    let pows = 1 : map (mulmod gen)  pows
    in pows

listModSum (x:xs) (y:ys) = ( (x + y) `mod` base) : listModSum xs ys

listFunModSum f [] = let zeros = 0:zeros in zeros
listFunModSum f (x:xs) = listModSum (f x) $ listFunModSum f xs

-- listDunMOdSum f = foldr (listModSum . f) (let zeros = 0:zeros in zeros)

powerNotes oct gens = map (num2note oct) $ listFunModSum powers gens

finalNotes gens oct tm = map ($tm) $ powerNotes oct gens

toPlay gens oct tm = line $ finalNotes gens oct tm