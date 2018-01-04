module Euler.P040.Problem049
  ( prob049
  )
  where

{-
 - The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
 - increases by 3330, is unusual in two ways:
 - (i) each of the three terms are prime, and,
 - (ii) each of the 4-digit numbers are permutations of one another.
 -
 - There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
 - exhibiting this property, but there is one other 4-digit increasing sequence.
 -
 - What 12-digit number do you form by concatenating the three terms in this sequence?
 -}

import Data.List           (permutations, sort)
import Data.Numbers.Primes (primes, isPrime)
import Data.Digits         (digits, unDigits)

{-prob049 :: Integer-}
prob049 = prob049' 4

-- naive method

{-prob049' :: Integer -> Integer-}
prob049' (fromIntegral->limit) = unDigits 10 . concatMap (digits 10) . head . head . filter (/= []) . fmap l $ avail
  where
    avail = takeWhile ((== limit) . length . digits 10) . dropWhile ((< limit) . length . digits 10) $ primes
    l n = filter (all (p n)) [take 3 [n,m..] | m <- (fmap (unDigits 10) . permutations . digits 10) n
                                             , n < m, n /= 1487]
    p n m = isPrime m && sort (digits 10 n) == sort (digits 10 m)
