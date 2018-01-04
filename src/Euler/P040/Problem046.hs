module Euler.P040.Problem046
  ( prob046
  )
  where

{-
 - It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
 -
 - 9 = 7 + 2×1^2
 - 15 = 7 + 2×2^2
 - 21 = 3 + 2×3^2
 - 25 = 7 + 2×3^2
 - 27 = 19 + 2×2^2
 - 33 = 31 + 2×1^2
 -
 - It turns out that the conjecture was false.
 -
 - What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
 -}

import Data.Maybe            (isJust)
import Data.Numbers.Primes   (isPrime, primes)
import Euler.P000.Problem009 (toInt)

prob046 :: Integer
prob046 = prob046' 1

-- naive method

prob046' :: Integer -> Integer
prob046' (fromIntegral->i) = (!! i) . filter (not . p) $ [1,3..]
  where
    p n = isPrime n || (any (isJust @Integer) ((fmap (toInt . sqrt . (/ 2) . (-) (d n) . fromIntegral)) (takeWhile (< n) primes)))
    d n = fromIntegral n :: Double
