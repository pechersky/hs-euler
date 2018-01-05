module Euler.P050.Problem053
  ( prob053
  )
  where

{- There are exactly ten ways of selecting three from five, 12345:
 -
 - 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
 -
 - In combinatorics, we use the notation, 5C3 = 10.
 -
 - In general,
 -
 - nCr =
 - n!
 - r!(n−r)!
 - ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
 - It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
 -
 - How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100,
 - are greater than one-million? -}

prob053 :: Integer
prob053 = prob053' 1e6

-- naive method
--
-- use log nCk = log (n! / k! (n-k)!) = log (n!) - log (k!) - log ((n-k)!)

prob053' :: Integer -> Integer
prob053' (fromIntegral->limit) = fromIntegral . length
               $ [(a,b) | a <- [1..100], b <- [1..a] , logC a b >= log limit]

factorial :: Integer -> Integer
factorial = product . enumFromTo 1

logC :: Integer -> Integer -> Double
logC n k = lf n - lf k - lf (n-k)
  where
    lf = log . fromIntegral . factorial
