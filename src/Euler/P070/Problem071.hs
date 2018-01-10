module Euler.P070.Problem071
  ( prob071
  )
  where

{-
 - Consider the fraction, n/d, where n and d are positive integers.
 - If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
 -
 - If we list the set of reduced proper fractions for d ≤ 8 in ascending order
 - of size, we get:
 -
 - 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7,
 - 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
 -
 - It can be seen that 2/5 is the fraction immediately to the left of 3/7.
 -
 - By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending
 - order of size, find the numerator of the fraction immediately to the left of 3/7.
 -}

import Data.Ratio ((%), numerator, denominator)

prob071 :: Integer
prob071 = prob071' 1e6

-- naive method

prob071' :: Integer -> Integer
prob071' limit = numerator . maximum . fmap f $ [1..limit]
  where
    bound = 3 % 7
    f x = (x * numerator bound - 1) `div` denominator bound % x
