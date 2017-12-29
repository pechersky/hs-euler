module Euler.P000.Problem003
  ( prob003
  )
  where

import Data.Numbers.Primes (primeFactors)

{- The prime factors of 13195 are 5, 7, 13 and 29.
 -
 - What is the largest prime factor of the number 600851475143 ? -}

prob003 :: Integer
prob003 = prob003' 600851475143

-- naive method

prob003' :: Integral int => int -> int
prob003' = maximum . primeFactors
