module Euler.P030.Problem035
  ( prob035
  )
  where

{- The number, 197, is called a circular prime because all rotations of the digits:
 - 197, 971, and 719, are themselves prime.
 -
 - There are thirteen such primes below 100:
 - 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
 -
 - How many circular primes are there below one million? -}

import Data.Numbers.Primes (primes, isPrime)

prob035 :: Integer
prob035 = prob035' 1e6

-- naive method

prob035' :: Integer -> Integer
prob035' limit = fromIntegral . length . filter circular . takeWhile (< limit) $ primes
  where
    circular n = all isPrime . fmap (read @Integer). permute . show $ n
    permute xs = take (length xs) . fmap (take (length xs)) . iterate (drop 1) . cycle $ xs
