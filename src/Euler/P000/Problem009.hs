module Euler.P000.Problem009
  ( prob009, pythagoreans, toInt
  )
  where

import Data.Maybe    (isJust, fromJust, listToMaybe)
import Control.Monad (guard)

{- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 -
 - a^2 + b^2 = c^2
 - For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc. -}

prob009 :: Maybe Integer
prob009 = prob009' 1000

-- naive method

prob009' :: Integer -> Maybe Integer
prob009' match = listToMaybe
               . fmap (\(a,b,c) -> a * b * c)
               . filter (\(a,b,c) -> a + b + c == match)
               . takeWhile (\(a,_,_) -> a < match) $ pythagoreans


pythagoreans :: [(Integer, Integer, Integer)]
pythagoreans = do
  a <- [2..]
  b <- [1..a]
  let c = toInt (sqrt . fromIntegral $ sq a + sq b :: Double)
  guard (isJust c)
  pure (a, b, fromJust c)
  where
    sq = (^ (2 :: Integer))

--Returns if x is an int to n decimal places

toInt :: (RealFrac a, Integral b) => a -> Maybe b
toInt x
  | isInt' x 7 = Just (round x)
  | otherwise  = Nothing

isInt' :: (RealFrac a) => a -> Integer -> Bool
isInt' x n = round (10^nint * (x - xint)) == (0 :: Integer)
  where
    nint = fromIntegral n :: Integer
    xint = fromIntegral (round x :: Integer)
