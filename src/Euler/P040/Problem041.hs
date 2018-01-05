module Euler.P040.Problem041
  ( prob041
  )
  where

{-
 - We shall say that an n-digit number is pandigital if it makes use of all the
 - digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
 -
 - What is the largest n-digit pandigital prime that exists?
 -}

import Data.List           (sort)
import Data.Digits         (digits)
import Data.Numbers.Primes (primes)

prob041 :: Integer
prob041 = prob041' 9

-- naive method

prob041' :: Integer -> Integer
prob041' limit = fromIntegral . last . filter pandigital . takeWhile (< 10 ^ limit) $ primes
  where
    pandigital n = let d = digits 10 n in sort d == [1..length d]
