module Euler.P040.Problem048
  ( prob048
  )
  where

{-
 - The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
 -
 - Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
 -}

import Data.Digits (digits, unDigits)

prob048 :: Integer
prob048 = prob048' 1000

-- naive method

prob048' :: Integer -> Integer
prob048' = unDigits 10 . reverse . take 10 . reverse . digits 10 . sum . fmap (\x -> x ^ x) . enumFromTo 1
