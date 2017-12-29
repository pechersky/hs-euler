module Euler.P000.Problem006
  ( prob006
  )
  where

{- The sum of the squares of the first ten natural numbers is,
 -
 - 12 + 22 + ... + 102 = 385
 - The square of the sum of the first ten natural numbers is,
 -
 - (1 + 2 + ... + 10)2 = 552 = 3025
 - Hence the difference between the sum of the squares of the first ten natural numbers
 - and the square of the sum is 3025 − 385 = 2640.
 -
 - Find the difference between the sum of the squares of the first one hundred natural
 - numbers and the square of the sum. -}

prob006 :: Integer
prob006 = prob006' 100

-- naive method

prob006' :: Integer -> Integer
prob006' n = sqsum n - sumsq n
  where
    sqsum = sq . sum . enumFromTo 1
    sumsq = sum . fmap sq . enumFromTo 1
    sq = (^ (2 :: Integer))
