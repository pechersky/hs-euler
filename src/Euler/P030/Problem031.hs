module Euler.P030.Problem031
  ( prob031
  )
  where

{- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
 -
 - 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 - It is possible to make £2 in the following way:
 -
 - 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 - How many different ways can £2 be made using any number of coins? -}

import Control.Monad (guard)

prob031 :: Integer
prob031 = prob031' 200

-- naive method

prob031' :: Integer -> Integer
prob031' value = fromIntegral . length $ go value coins [[]]
  where
    coins = [200, 100, 50, 20, 10, 5, 2, 1]
    go 0 _ xs = xs
    go _ [] _ = []
    go n (c:cs) xs = go n cs xs ++ do
      subxs <- xs
      guard (c + sum subxs <= value)
      go (n - c) (c:cs) (return (c : subxs))
