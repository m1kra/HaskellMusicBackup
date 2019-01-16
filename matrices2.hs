import Data.Matrix
import Euterpea


-- m = fromLists [ [2, 1], [1, 1] ]
-- m = fromLists [[1, 1], [-1, 1]]
m = fromLists [[-1, 1], [1, -1]]

notes oct = map ($oct) [c, d, e, f, g, a, b]

base = 35

notes1 = foldr (:)  (foldr (:) (notes 1) (notes 2)) (notes 3)

moreNotes = foldr(:) (foldr (:) notes1 (notes 4)) (notes 5)

num2note num tm = ($tm) $ moreNotes !! abs (num `rem` base)

powers = identity 2 : map (*m) powers

m2m1 m = num2note (getElem 1 1 m) en
m2m2 m = num2note (getElem 1 2 m) qn
m2m3 m = num2note (getElem 2 1 m) qn
m2m4 m = num2note (getElem 2 2 m) en

mm1 = map m2m1 powers
mm2 = map m2m2 powers
mm3 = map m2m3 powers
mm4 = map m2m4 powers

musify [x] = x
musify (x:xs) = x :+: musify xs

toPlay = (musify mm1) :=: (musify mm2) :=: (musify mm3) :=: (musify mm4)



ens = map pitch . drop 16 . take 49 $ ens'
    where ens' = 0:1:3:4:6:7:9:10: map (+12) ens'



wts = map pitch . drop 16 . take 49 $ wts'
    where wts' = 0:2:4:6:8:10: map (+12) wts'