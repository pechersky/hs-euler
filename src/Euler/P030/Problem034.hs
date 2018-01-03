module Euler.P030.Problem034
  ( prob034
  )
  where

{- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 -
 - Find the sum of all numbers which are equal to the sum of the factorial of their digits.
 -
 - Note: as 1! = 1 and 2! = 2 are not sums they are not included. -}

import Data.Digits (digits)

prob034 :: Integer
prob034 = prob034' (factorial 9)

-- naive method

prob034' :: Integer -> Integer
prob034' = sum . filter digitSum . enumFromTo 3
  where
    digitSum n = n == (sum . fmap factorial . digits 10) n

factorial :: Integer -> Integer
factorial = product . enumFromTo 1
