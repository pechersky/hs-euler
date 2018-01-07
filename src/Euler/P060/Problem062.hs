{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module Euler.P060.Problem062
  ( prob062
  )
  where

{-
 - The cube, 41063625 (3453), can be permuted to produce two other cubes:
 - 56623104 (3843) and 66430125 (4053).
 - In fact, 41063625 is the smallest cube which has exactly three permutations
 - of its digits which are also cube.
 -
 - Find the smallest cube for which exactly five permutations of its digits are cube.
 -}

import qualified Data.Map.Strict as M
import           Data.List       (sort)
import           Data.Digits     (digits)
import           Control.Lens

prob062 :: Integer
prob062 = prob062' 5

-- naive method

prob062' :: Integer -> Integer
prob062' (fromIntegral->limit) = go [1..] M.empty
  where
    go [] _ = 0
    go (v:vs) !m
      | vs == []  = v
      | (limit - 1) == length (m ^. at key . non []) = cube $ minimum (m ^. at key . non [])
      | otherwise = go vs nm
      where
        cube = (^ (3::Integer))
        key = sort . digits 10 . cube $ v
        nm = m & at key <>~ (Just [v])
