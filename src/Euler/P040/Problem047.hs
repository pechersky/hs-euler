module Euler.P040.Problem047
  ( prob047
  )
  where

{-
 - The first two consecutive numbers to have two distinct prime factors are:
 -
 - 14 = 2 × 7
 - 15 = 3 × 5
 -
 - The first three consecutive numbers to have three distinct prime factors are:
 -
 - 644 = 2² × 7 × 23
 - 645 = 3 × 5 × 43
 - 646 = 2 × 17 × 19.
 -
 - Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
 -}

import Data.List           (tails, transpose, group)
import Data.Numbers.Primes (primeFactors)

prob047 :: Integer
prob047 = prob047' 4

-- naive method

prob047' :: Integer -> Integer
prob047' (fromIntegral->limit) = head . head . filter (all ((==) limit . length . group . primeFactors))
                               . transpose . take limit . tails $ [1..]
