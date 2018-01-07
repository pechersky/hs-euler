module Euler.P050.Problem057
  ( prob057
  )
  where

{-
 - It is possible to show that the square root of two can be expressed as an
 - infinite continued fraction.
 -
 - √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
 -
 - By expanding this for the first four iterations, we get:
 -
 - 1 + 1/2 = 3/2 = 1.5
 - 1 + 1/(2 + 1/2) = 7/5 = 1.4
 - 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
 - 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
 -
 - The next three expansions are 99/70, 239/169, and 577/408, but the eighth
 - expansion, 1393/985, is the first example where the number of digits in the
 - numerator exceeds the number of digits in the denominator.
 -
 - In the first one-thousand expansions, how many fractions contain a numerator
 - with more digits than denominator?
 -}

import Data.Ratio  (numerator, denominator, Rational)
import Data.Digits (digits)

prob057 :: Integer
prob057 = prob057' 1000

-- naive method

prob057' :: Integer -> Integer
prob057' (fromIntegral->limit) = fromIntegral . length . filter match . take limit . iterate fsum $ 1
  where
    fsum n = 1 + 1 / (1 + n)
    match :: Rational -> Bool
    match n = length (digits 10 (numerator n)) > length (digits 10 (denominator n))
