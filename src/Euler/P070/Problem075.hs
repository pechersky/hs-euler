module Euler.P070.Problem075
  ( prob075
  )
  where

{- Problem statement here -}

import Data.Maybe            (fromJust, isJust)
import Control.Monad         (guard)
import Euler.P000.Problem009 (toInt)

{- prob075 :: Integer -}
prob075 = prob075' 15000

-- naive method

{- prob075' :: Integer -> Integer -}
prob075' limit = do
  c <- takeWhile (< (fromIntegral (sq limit))) squares
  b <- takeWhile (< c) squares
  let a = c - b
  guard (b > a)
  guard (isJust (toInt (sqrt a)))
  guard (f a + f b + f c <= limit)
  pure (f a, f b, f c)
  where
    sq = (^ (2 :: Integer))
    f = fromJust . toInt . sqrt

whead [] = []
whead (x:xs) = [x]

squares = fmap sq [2..]
  where
    sq = (^ (2 :: Integer))

