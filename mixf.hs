
{-# LANGUAGE TemplateHaskell #-}
import Euterpea hiding (f)
import Data.Ratio

unit = 32
toTempo rational = 1 % ((abs rational `mod` unit) + 1)
toPitch int = pitch $ fromInteger $ (abs int) `mod` 100
musify rational int = note (toTempo rational) $ toPitch int
zipf f (x:xs) (y:ys) = f x y : zipf f xs ys 

f x y = y * y - x * x * x  + x - 1
xsf n = n*n - n `mod` 100
ysf n = n*n + n `mod` 100
xs = map xsf [1..]
ys = map ysf [1..]

mySeq = zipf f ys xs
qns = 4 : qns

musics = zipf musify qns mySeq

join [x] = x
join (x:xs) = x :+: join xs

toPlay = join $ take 500 musics

