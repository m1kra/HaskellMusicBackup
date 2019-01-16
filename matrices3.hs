{-# LANGUAGE TemplateHaskell #-}
import Data.Matrix
import Euterpea
import Data.Ratio


-- m = fromLists [[2, 1] , [1, 2]]
-- v = fromLists [[1, -1]]
-- w = fromLists [[1, 2]]
-- n = 2

m = fromLists [[1, 2, 4], [1, 3, 9], [1, 1, 1]]
v = fromLists [[1, 1, 1]]
w = fromLists [[1, 10, 10]]
n = 3

m2 = fromLists [[-13, 8, 2, -5], [-1, 3, -3, 1], [2, 4, 6, -8], [-1, 3, 5, 7]]
v2 = fromLists [[12, 11, 11, 1]]
w2 = fromLists [[1, 3, 5, 7]]
n2 = 4

-- m = fromLists [[1, 2, 3, 4], [4, 3, 2, 1], [-1, -2, -3, -4], [-4, -3, -2, 7]]
-- v = fromLists [[12, 11, -11, -1]]
-- w = fromLists [[1, 3, 5, 7]]
-- n = 4

-- m = fromLists [[1, 0, 3, 1, 4], [0, 3, 0, 1, 5], [0, -2, 3, 0, 6], [4, 0, 0, 7, 7], [1, 2, 3, 4, 5]]
-- v = fromLists [[1, 2, 3, -4, 5]]
-- w = fromLists [[3, 2, 1, 4, -1]]
-- n = 5

unit = 32
nn n = toInteger n
toTempo rational n = 1 % ((abs (rational - (toInteger n) ) `mod` unit) + 1)
-- toPitch int = pitch $ fromInteger $ (abs int) `mod` 100
eightnote = 1:2:4:5:7:8:9:11: map (+12) eightnote
toPitch = pitch . (eightnote !!) . fromInteger . (+16) . (`mod` 88)
musify rational int n = note (toTempo rational n) $ toPitch int



powers m n = identity n : map (multStd m) (powers m n)
vImages v m = map (v * ) . powers m
-- toList v = map (\_ -> getElem 1 _ v) [1..n]


toMusic  n [x] [y] = musify x y n
toMusic n (x:xs) (y:ys) = musify x y n :=: toMusic n xs ys

zipf f (x:xs) (y:ys) = f x y : zipf f xs ys 
musics v w m n = zipf (toMusic n) (map toList $ vImages v m n) (map toList $ vImages w m n)

-- join [x] = x
-- join (x:xs) = x :+: join xs

toPlay = chord . map (line . take 500)  $ [musics v w m n, musics v2 w2 m2 n2]


