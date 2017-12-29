module Euler.Factors
  ( toFactors, runFactors, numDivisors
  )
  where

import           Data.List           (group)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Control.Arrow       ((&&&))
import           Data.Numbers.Primes (primeFactors)

newtype Factors = Factors {unFactors :: IntMap Int}
  deriving (Show)

instance Num Factors where
  (+) (Factors f) (Factors g) = Factors $ IM.unionWith max f g
  (-) (Factors f) (Factors g) = Factors $ IM.unionWith min f g
  (*) (Factors f) (Factors g) = Factors $ IM.unionWith (+) f g
  negate = id
  abs = id
  signum = id
  fromInteger = toFactors . fromIntegral

instance Enum Factors where
  toEnum = toFactors
  fromEnum = runFactors

toFactors :: Int -> Factors
toFactors n
  | n < 1 = error "non-positive not supported"
  | otherwise = (Factors . IM.fromList . bin . primeFactors) n
  where
    bin = fmap (head &&& length) . group

runFactors :: Factors -> Int
runFactors = IM.foldrWithKey (\k v acc -> acc * (k ^ v)) 1 . unFactors

numDivisors :: Factors -> Int
numDivisors = IM.foldr (\v acc -> acc * (v + 1)) 1 . unFactors
