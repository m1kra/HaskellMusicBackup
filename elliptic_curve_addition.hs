import Euterpea hiding (a)
import Data.Ratio

unit = 32
toTempo x = 1 % (abs x + 1)
toPitch = pitch . fromInteger . abs
musify (rational, int) = note (toTempo rational) $ toPitch int

-- y^2 = x^3 + ax + b
a = 1
b = 2


lambda (x, y) (z, t) = div (t - y)  (z - x)
xr (x, y) (z, t) = lambda (x, y) (z, t) ^ 2 - x - z
s (x, y) = div (3 * x * x + a)  (2 * y)
xd (x, y) = s (x, y)^2 - 2 * x

doublePoint (x, y) = ( xd (x, y), y + s (x, y) * (xd (x, y) - x))
addDifferentPoints (x, y) (z, t) = (xr (x, y) (z, t), lambda (x, y) (z, t) * (x - (xr (x, y) (z, t))) + y)

-- addPoints :: (Integral t) => (Ratio t, Ratio t) -> (Ratio t, Ratio t) -> (Ratio t, Ratio t)
addPoints (x, y) (z, t) = if x == z then doublePoint (x, y) else addDifferentPoints (x, y) (z, t)

p :: (Integer, Integer)
p = (2, 1)
-- q = doublePoint p

-- -- points :: (Integral a, Integral t) => a -> (Ratio t, Ratio t)
mults :: [(Integer, Integer)]
mults = p : map doublePoint mults

join [x] = x
join (x:xs) = x :+: join xs

toPlay = join $ map musify mults

