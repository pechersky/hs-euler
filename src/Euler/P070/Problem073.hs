module Euler.P070.Problem073
  ( prob073
  )
  where

{-
 - Consider the fraction, n/d, where n and d are positive integers. If n<d and
 - HCF(n,d)=1, it is called a reduced proper fraction.
 -
 - If we list the set of reduced proper fractions for d ≤ 8 in ascending order
 - of size, we get:
 -
 - 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2,
 - 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
 -
 - It can be seen that there are 3 fractions between 1/3 and 1/2.
 -
 - How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper
 - fractions for d ≤ 12,000?
 -}

import Data.Ratio ((%), numerator, denominator)

prob073 :: Integer
prob073 = prob073' 12000

-- naive method

prob073' :: Integer -> Integer
prob073' limit = fromIntegral . length . concat . fmap (\x -> filter (p x) [g x .. f x]) $ [2..limit]
  where
    upperbound = 1 % 2
    lowerbound = 1 % 3
    f x = ceiling @Double (fromIntegral (x * numerator upperbound) / fromIntegral (denominator upperbound))
    g x = floor @Double (fromIntegral (x * numerator lowerbound) / fromIntegral (denominator lowerbound))
    p x y = let r = y % x in r > lowerbound && r < upperbound && x == denominator r
