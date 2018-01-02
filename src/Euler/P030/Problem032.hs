module Euler.P030.Problem032
  ( prob032
  )
  where

{- We shall say that an n-digit number is pandigital if it makes use of all the
 - digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
 -
 - The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
 - multiplicand, multiplier, and product is 1 through 9 pandigital.
 -
 - Find the sum of all products whose multiplicand/multiplier/product identity
 - can be written as a 1 through 9 pandigital.
 -
 - HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum. -}

import           Data.List       (permutations)
import           Data.List.Split (splitPlaces)
import qualified Data.IntSet     as IS

prob032 :: Integer
prob032 = prob032' 9

-- naive method

prob032' :: Integer -> Integer
prob032' limit = fromIntegral . sum . IS.toList . IS.fromList . fmap (\(_,_,x) -> x) . filter match $ pandigitals
  where
    match (a,b,x) = a * b == x
    pandigitals = (fmap ((\(a:b:x:_) -> (a,b,x)) . fmap (read . concat)) . splitPlaces)
              <$> splitDigits <*> (permutations . fmap show) [1.. limit]
    splitDigits = [[a,b,c] | a <- [1..limit], b <- [1..a]
                           , let c = limit - (a + b), a + b < (c + 2)]
