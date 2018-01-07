module Euler.P050.Problem056
  ( prob056
  )
  where

{-
 - A googol (10^100) is a massive number: one followed by one-hundred zeros;
 - 100^100 is almost unimaginably large: one followed by two-hundred zeros.
 - Despite their size, the sum of the digits in each number is only 1.
 -
 - Considering natural numbers of the form, a^b, where a, b < 100,
 - what is the maximum digital sum?
 -}

import Data.Digits (digits)

prob056 :: Integer
prob056 = prob056' 100

-- naive method

prob056' :: Integer -> Integer
prob056' limit = maximum . fmap (sum . digits 10) $ [a ^ b | a <- [1..limit], b <- [1..limit]]
