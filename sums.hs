import Euterpea

gen = 2

base = 21


notes oct = map ($oct) [c, d, e, f, g, a, b]

moreNotes = foldr (:)  (foldr (:) (notes 1) (notes 2)) (notes 3)

num2note num = moreNotes !! num

mulmod x y = (x * y) `mod` base

powers gen = 
    let pows = 1 : map (mulmod gen)  pows
    in pows

listModSum (x:xs) (y:ys) = ( (x + y) `mod` base) : listModSum xs ys

listFunModSum f [] = let zeros = 0:zeros in zeros
listFunModSum f (x:xs) = listModSum (f x) $ listFunModSum f xs

-- listDunMOdSum f = foldr (listModSum . f) (let zeros = 0:zeros in zeros)

powerNotes gens = map num2note $ listFunModSum powers gens

finalNotes gens tm = map ($tm) $ powerNotes gens

toPlay gens tm = line $ finalNotes gens tm
