module Euler.P060.Problem060
  ( prob060
  )
  where

{-
 - The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
 - and concatenating them in any order the result will always be prime.
 - For example, taking 7 and 109, both 7109 and 1097 are prime.
 - The sum of these four primes, 792, represents the lowest sum for a set of
 - four primes with this property.
 -
 - Find the lowest sum for a set of five primes for which any two primes
 - concatenate to produce another prime.
 -}

import Data.Numbers.Primes (primes, isPrime)
import Data.Digits (digits, unDigits)

prob060 :: Integer
prob060 = prob060'

-- naive method

prob060' :: Integer
prob060' = sum . head $ do
  a <- ps
  let as = filter (valid a) . dropWhile (<= a) $ ps
  b <- as
  let bs = filter (valid b) . dropWhile (<= b) $ as
  c <- bs
  let cs = filter (valid c) . dropWhile (<= c) $ bs
  d <- cs
  let ds = filter (valid d) . dropWhile (<= d) $ cs
  e <- ds
  pure [a,b,c,d,e]
    where
      ps = takeWhile (< 1e4) $ primes

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

mix :: Integer -> Integer -> [Integer]
mix a b = unDigits 10 <$> [da ++ db, db ++ da]
  where
    da = digits 10 a
    db = digits 10 b

valid :: Integer -> Integer -> Bool
valid a b = all isPrime $ mix a b
