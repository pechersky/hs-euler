module Euler.P020.Problem024
  ( prob024
  )
  where

{-
 -A permutation is an ordered arrangement of objects.
 -For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
 -If all of the permutations are listed numerically or alphabetically,
 -we call it lexicographic order.
 -The lexicographic permutations of 0, 1 and 2 are:
 -
 -012   021   102   120   201   210
 -
 -What is the millionth lexicographic permutation of the
 -digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 -}

import Data.List (sort, permutations)

prob024 :: String
prob024 = prob024' (1e6 - 1)

-- naive method

prob024' :: Integer -> String
prob024' = (sort (permutations "0123456789") !!) . fromIntegral
