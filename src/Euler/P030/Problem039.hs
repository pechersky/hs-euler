module Euler.P030.Problem039
  ( prob039
  )
  where

{- If p is the perimeter of a right angle triangle with integral length sides,
 - {a,b,c}, there are exactly three solutions for p = 120.
 -
 - {20,48,52}, {24,45,51}, {30,40,50}
 -
 - For which value of p ≤ 1000, is the number of solutions maximised? -}

import Data.Ord              (comparing)
import Data.List             (maximumBy)
import Euler.P000.Problem009 (pythagoreans)

prob039 :: Integer
prob039 = prob039' 1000

-- naive method

prob039' :: Integer -> Integer
prob039' = maximumBy (comparing solutions) . enumFromTo 1
  where
    tsum (a,b,c) = a + b + c
    solutions n = length . filter ((== n) . tsum) . takeWhile ((<= 2 * n) . tsum) $ pythagoreans
