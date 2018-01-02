module Euler.P020.Problem026
  ( prob026, lencycle
  )
  where

{-
 -A unit fraction contains 1 in the numerator.
 -The decimal representation of the unit fractions with denominators 2 to 10 are given:
 -
 -1/2 = 0.5
 -1/3 = 0.(3)
 -1/4 = 0.25
 -1/5 = 0.2
 -1/6 = 0.1(6)
 -1/7 = 0.(142857)
 -1/8 = 0.125
 -1/9 = 0.(1)
 -1/10 = 0.1
 -Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle.
 -It can be seen that 1/7 has a 6-digit recurring cycle.
 -
 -Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
 -}

import Data.List (maximumBy)
import Data.Ord  (comparing)

prob026 :: Integer
prob026 = prob026' (1e3 - 1)

-- naive method

prob026' :: Integer -> Integer
prob026' = fst . maximumBy (comparing snd) . fmap (\x -> (x, lencycle x)) . enumFromTo 1

lencycle :: Integer -> Int
lencycle n
  | n == 1 = 1
  | n `mod` 2 == 0 = lencycle (n `div` 2)
  | n `mod` 5 == 0 = lencycle (n `div` 5)
  | otherwise = length . show . head . dropWhile ((/= 0) . (`mod` n))
              . fmap (read . (flip (replicate) '9')) $ [1..]
