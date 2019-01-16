import Data.Matrix
import Euterpea


-- m = fromLists [ [1, 1], [-1, 1] ]
m = fromLists [[1, 2], [-1, 1]]

notes = [c, d, e, f, g, a, b]

num2note num oct tm = ($tm) . ($oct) $ notes !! abs (num `rem` 7)

powers = identity 2 : map (*m) powers

matrix2music m = num2note (getElem 1 1 m) 2 qn :=: num2note (getElem 1 2 m) 3 qn :+: num2note (getElem 2 1 m) 2 en :=: num2note (getElem 2 2 m) 3 en

musicMatrices = map matrix2music powers

musify [x] = x
musify (x:xs) = x :+: musify xs

toPlay = musify $ take 100 musicMatrices


-- m = fromLists [ [1, 1, 1], [-1, 1, 1], [-1, -1, 1] ]
-- v = Data.Matrix.transpose $ fromLists [[1, 1, 1, 1]]
-- n = 3

-- notes = [c, d, e, f, g, a, b]

-- num2note num oct tm = ($tm) . ($oct) $ notes !! abs (num `rem` 7)

-- powers = identity 2 : map (*m) powers

-- vec2list vec = map (\x -> getElem 1 x vec) [1..n]
-- octaves = [1, 2, 3, 4, 5]

-- _list2music x i = (i `mod` 4) . ($qn)  (x !! i)
-- list2music x = map (_list2music x) [1..n] 

-- -- vectors = map vec2list . (*v) powers

-- -- vector2music vec = map (num2music)

-- -- matrix2music =  [1..n] 

-- -- musicMatrices = map matrix2music powers

-- -- musify [x] = x
-- -- musify (x:xs) = x :+: musify xs

-- -- toPlay = musify $ take 30 musicMatrices
