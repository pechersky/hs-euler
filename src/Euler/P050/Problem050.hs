module Euler.P050.Problem050
  ( prob050
  )
  where

{-
 - The prime 41, can be written as the sum of six consecutive primes:
 -
 - 41 = 2 + 3 + 5 + 7 + 11 + 13
 - This is the longest sum of consecutive primes that adds to a prime below one-hundred.
 -
 - The longest sum of consecutive primes below one-thousand that adds to a prime,
 - contains 21 terms, and is equal to 953.
 -
 - Which prime, below one-million, can be written as the sum of the most consecutive primes?
 -}

import Data.List           (tails, transpose)
import Data.Numbers.Primes (primes, isPrime)

prob050 :: Integer
prob050 = prob050' 1e6

-- naive method

prob050' :: Integer -> Integer
prob050' limit = last . last . filter (/= []) . fmap (filter isPrime) . takeWhile (/= [])
               . fmap (takeWhile (<= limit)) . transpose $ primesSums

primesSums :: [[Integer]]
primesSums = fmap (scanl1 (+)) . tails $ primes
