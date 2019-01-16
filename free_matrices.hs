{-# LANGUAGE TemplateHaskell #-}
import Data.Matrix
import Euterpea
import Data.Ratio

-- m = fromLists [[1, 1] , [-1, 0]]
-- n = 2
m = fromLists [[2, 2] , [-2, 0]]
n = 2
-- m = fromLists [[1, 1], [0, 1]]
-- n = 2
-- m = fromLists [[1, 1, 1], [0, 1, 1], [0, 0, 1]]
-- n = 3
-- m = fromLists [[1, 1, 0], [1, 0, 1], [0, 0, 1]]
-- n = 3
-- m = fromLists [[1, 0, 1], [1, 1, 1], [0, 1, 0]]
-- n = 3
-- m = fromLists [[-13, 8, 2, -5], [-1, 3, -3, 1], [2, 4, 6, -8], [-1, 3, 5, 7]]
-- n = 4
-- m = fromLists [[1, 2, 3, 4], [4, 3, 2, 1], [-1, -2, -3, -4], [-4, -3, -2, 7]]
-- n = 4
-- m = fromLists [[1, 0, 3, 0], [0, 3, 0, 1], [0, -2, -3, 0], [-4, 0, 0, 7]]
-- n = 4
unit = 32

nn = toInteger n
toTempo rational = 1 % ((abs (rational - nn) `mod` unit) + 1)
toPitch int = pitch $ fromInteger $ (abs int) `mod` 100
musify rational int = note (toTempo rational) $ toPitch int

powers = identity n : map (multStd m) powers

sumCol i m = sum $ map (\k -> getElem k i m) [1..n]
colSums m = map (\i -> sumCol i m) [1..n]

sumRow i m = sum $ map (\k -> getElem i k m) [1..n]
rowSums m = map (\i -> sumRow i m) [1..n]

min x y = if x < y then x else y
minList [x] = x
minList (x:xs) = Main.min x $ minList xs

minAbsRow i m = minList $ map (abs . (\k -> getElem i k m)) [1..n]
minAbsRows m = map (\i -> minAbsRow i m) [1..n]

toMusic [x] [y] = musify x y
toMusic (x:xs) (y:ys) = musify x y :=: toMusic xs ys
-- toMusic (x:xs) (y:ys) = musify x y :+: toMusic xs ys


zipf f (x:xs) (y:ys) = f x y : zipf f xs ys 

musics = zipf toMusic (map rowSums powers) (map colSums powers)
-- musics = zipf toMusic (map minAbsRows powers) (map colSums powers)

join [x] = x
join (x:xs) = x :+: join xs

toPlay = join $ take 500 musics

