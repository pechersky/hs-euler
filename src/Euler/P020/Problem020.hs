module Euler.P020.Problem020
  ( prob020
  )
  where

{-
 -n! means n × (n − 1) × ... × 3 × 2 × 1
 -
 -For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
 -and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 -
 -Find the sum of the digits in the number 100!
 -}

import Data.Digits (digits)

prob020 :: Integer
prob020 = prob020' 100

-- naive method

prob020' :: Integer -> Integer
prob020' = sum . digits 10 . factorial

factorial :: Integer -> Integer
factorial = product . enumFromTo 1
