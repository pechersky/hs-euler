module Euler.P010.Problem014
  ( prob014
  )
  where

{- The following iterative sequence is defined for the set of positive integers:
 -
 - n → n/2 (n is even)
 - n → 3n + 1 (n is odd)
 -
 - Using the rule above and starting with 13, we generate the following sequence:
 -
 - 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 - It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
 - Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 -
 - Which starting number, under one million, produces the longest chain?
 -
 - NOTE: Once the chain starts the terms are allowed to go above one million. -}

import           Data.List        (unfoldr, maximumBy)
import           Data.Ord         (comparing)
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IM

prob014 :: Integer
prob014 = prob014' 1e6

-- naive method

prob014' :: Integer -> Integer
prob014' = toInteger . fst . maximumBy (comparing snd) . IM.toList . lencollatz . fromIntegral
{- prob014' _ = undefined -}

collatz :: Integer -> [Integer]
collatz = (++ [1]) . unfoldr go
  where
    go 1 = Nothing
    go n
      | even n    = Just (n, n `div` 2)
      | otherwise = Just (n, 3 * n + 1)

lencollatz :: Int -> IntMap Int
lencollatz limit = imap
  where
    imap = IM.fromAscList [(i, go i) | i <- [1..limit]]
    go n
      | n == 1     = 1
      | n' > limit = 1 + go n'
      | otherwise  = 1 + imap IM.! n'
      where
        n' = if even n then n `div` 2 else 3 * n + 1
