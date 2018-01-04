module Euler.P040.Problem040
  ( prob040
  )
  where

{-
 -An irrational decimal fraction is created by concatenating the positive integers:
 -
 -0.123456789101112131415161718192021...
 -
 -It can be seen that the 12th digit of the fractional part is 1.
 -
 -If dn represents the nth digit of the fractional part, find the value of the following expression.
 -
 -d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
 -}

import Data.Digits (digits)

prob040 :: Integer
prob040 = prob040' 6

-- naive method

prob040' :: Integer -> Integer
prob040' = product . fmap ((champer !!) . subtract 1 . (10 ^)) . enumFromTo 0
  where
    champer = concatMap (digits 10) [1..]
