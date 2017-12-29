module Euler.P010.Problem010
  ( prob010
  )
  where

{- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 -
 - Find the sum of all the primes below two million. -}

import Data.Numbers.Primes (primes)

prob010 :: Integer
prob010 = prob010' 2e6

-- naive method

prob010' :: Integer -> Integer
prob010' limit = sum . takeWhile (< limit) $ primes
