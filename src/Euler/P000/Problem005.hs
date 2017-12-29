module Euler.P000.Problem005
  ( prob005
  )
  where

import           Data.List           (group)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Control.Arrow       ((&&&))
import           Data.Numbers.Primes (primeFactors)

{- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 -
 - What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? -}

prob005 :: Integer
prob005 = prob005' 20

-- naive method

prob005' :: Int -> Integer
prob005' bound = toInteger . runFactors . foldr (+) 1 . fmap (fromIntegral . fromEnum) $ [1..bound]

newtype Factors = Factors {unFactors :: IntMap Int}
  deriving (Show)

toFactors :: Int -> Factors
toFactors n
  | n < 1 = error "non-positive not supported"
  | otherwise = (Factors . IM.fromList . bin . primeFactors) n
  where
    bin = fmap (head &&& length) . group

runFactors :: Factors -> Int
runFactors = IM.foldrWithKey (\k v acc -> acc * (k ^ v)) 1 . unFactors

instance Num Factors where
  (+) (Factors f) (Factors g) = Factors $ IM.unionWith max f g
  (-) (Factors f) (Factors g) = Factors $ IM.unionWith min f g
  (*) (Factors f) (Factors g) = Factors $ IM.unionWith (+) f g
  negate = id
  abs = id
  signum = id
  fromInteger = toFactors . fromIntegral
