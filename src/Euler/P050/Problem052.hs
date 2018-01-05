module Euler.P050.Problem052
  ( prob052
  )
  where

{- It can be seen that the number, 125874, and its double, 251748,
 - contain exactly the same digits, but in a different order.
 -
 - Find the smallest positive integer, x, such that
 - 2x, 3x, 4x, 5x, and 6x, contain the same digits. -}

import Data.List   (sort)
import Data.Digits (digits)

prob052 :: Integer
prob052 = prob052' 6

-- naive method

prob052' :: Integer -> Integer
prob052' (fromIntegral->limit) = head . head . filter match . fmap (take limit . f) $ [1..]
  where
    f = scanl1 (+) . repeat
    match = and . (zipWith (==) <*> tail) . fmap (sort . digits 10)
