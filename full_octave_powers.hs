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

powerNotes gen = map num2note $ powers gen

finalNotes gen tm = map ($tm) $ powerNotes gen

toPlay gen tm = line $ finalNotes gen tm
