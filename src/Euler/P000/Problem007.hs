module Euler.P000.Problem007
  ( prob007
  )
  where

import Data.Numbers.Primes (primes)

{- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 -
 - What is the 10 001st prime number? -}

prob007 :: Integer
prob007 = prob007' 10001

-- naive method

prob007' :: Integer -> Integer
prob007' = (primes !!) . fromIntegral . subtract 1
