module Euler.P060.Problem066
  ( prob066
  )
  where

{-
 - Consider quadratic Diophantine equations of the form:
 -
 - x^2 – Dy^2 = 1
 -
 - For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.
 -
 - It can be assumed that there are no solutions in positive integers when D is square.
 -
 - By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
 -
 - 3^2 – 2×2^2 = 1
 - 2^2 – 3×1^2 = 1
 - 9^2 – 5×4^2 = 1
 - 5^2 – 6×2^2 = 1
 - 8^2 – 7×3^2 = 1
 -
 - Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
 -
 - Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
 -}

import Data.Maybe            (isNothing)
import Euler.P000.Problem009 (toInt)
import Euler.P060.Problem064 (expandSqrt)
import Euler.P060.Problem065 (sumConvergents)
import Data.Ratio            (numerator, denominator)
import Data.List             (inits, cycle, maximumBy)
import Data.Ord              (comparing)

prob066 :: Integer
prob066 = prob066' 1000

-- naive method

prob066' :: Integer -> Integer
prob066' limit = maximumBy (comparing f) . filter p $ [2..limit]
  where
    f = fst . minDiophantine
    p = isNothing @Integer . toInt @Double . sqrt @Double . fromIntegral @Integer

minDiophantine :: Integer -> (Integer, Integer)
minDiophantine d = head . filter ((==) 1 . uncurry f) . fmap g $ convergents
  where
    f x y = x * x - d * y * y
    g r = (numerator r, denominator r)
    convergents = let (x:xs) = expandSqrt d 0 1 in fmap (sumConvergents x) . inits . cycle $ xs
