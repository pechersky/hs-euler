module Euler.P020.Problem021
  ( prob021
  )
  where

{-
 -Let d(n) be defined as the sum of proper divisors of n
 -(numbers less than n which divide evenly into n).
 -If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
 -each of a and b are called amicable numbers.
 -
 -For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
 -therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 -
 -Evaluate the sum of all the amicable numbers under 10000.
 -}

import Euler.Factors (divisors)

prob021 :: Integer
prob021 = prob021' 10000

-- naive method

prob021' :: Integer -> Integer
prob021' limit = sum . filter amicable . enumFromTo 2 $ (limit - 1)
  where
    amicable n
      | n >= limit = False
      | n == (fromIntegral . (memoizedDivSum !!) . fromIntegral $ n) = False
      | otherwise  = n == (fromIntegral . (memoizedDivSum !!) . (memoizedDivSum !!) . fromIntegral $ n)

memoizedDivSum :: [Int]
memoizedDivSum = 1:1:fmap (sum . tail . divisors) [2..]
