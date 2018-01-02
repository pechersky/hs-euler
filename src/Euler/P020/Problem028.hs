module Euler.P020.Problem028
  ( prob028
  )
  where

{-
 -Starting with the number 1 and moving to the right in a
 -clockwise direction a 5 by 5 spiral is formed as follows:
 -
 -21 22 23 24 25
 -20  7  8  9 10
 -19  6  1  2 11
 -18  5  4  3 12
 -17 16 15 14 13
 -
 -It can be verified that the sum of the numbers on the diagonals is 101.
 -
 -What is the sum of the numbers on the diagonals
 -in a 1001 by 1001 spiral formed in the same way?
 -}

prob028 :: Integer
prob028 = prob028' 1001

-- naive method

prob028' :: Integer -> Integer
prob028' limit = sum . fmap (sum . corners) $ [1,3 .. limit]
  where
    corners 1 = [1]
    corners n = take 4 [n * n, n * n - (n - 1) ..]
