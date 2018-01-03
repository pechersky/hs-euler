module Euler.P030.Problem037
  ( prob037
  )
  where

{- The number 3797 has an interesting property. Being prime itself,
 - it is possible to continuously remove digits from left to right,
 - and remain prime at each stage: 3797, 797, 97, and 7.
 - Similarly we can work from right to left: 3797, 379, 37, and 3.
 -
 - Find the sum of the only eleven primes that are both truncatable
 - from left to right and right to left.
 -
 - NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes. -}

import Data.List           (inits, tails)
import Data.Numbers.Primes (primes, isPrime)
import Data.Digits         (digits, unDigits)

prob037 :: Integer
prob037 = prob037' 11

-- naive method

prob037' :: Integer -> Integer
prob037' (fromIntegral->limit) = sum . take limit . filter truncatable . dropWhile (<= 7) $ primes
  where
    truncatable = all (isPrime . unDigits 10) . concat . sequence [init . tails, tail . inits] . digits 10
