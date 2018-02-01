module Euler.Factors
  ( toFactors
  , runFactors
  , numDivisors
  , divisors
  , abundant
  , powerset
  , totient
  , coprimesTo
  , go
  )
  where

import           Data.List                        (group)
import           Data.List.Ordered                (nubSort)
import qualified Data.IntSet                      as IS
import           Data.IntMap.Strict               (IntMap)
import qualified Data.IntMap.Strict               as IM
import           Control.Applicative              (many)
import           Control.Monad                    (filterM, guard)
import           Control.Monad.Trans.State.Strict (get, put, evalStateT)
import           Control.Monad.Trans.Class        (lift)
import           Control.Arrow                    ((&&&))
import           Data.Numbers.Primes              (primeFactors, primes)

newtype Factors = Factors {unFactors :: IntMap Int}
  deriving (Show, Eq, Ord)

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

divisors :: Factors -> [Int]
divisors = IS.toDescList . IS.fromList . fmap product . powerset
         . concatMap ((uncurry . flip) replicate) . IM.assocs . unFactors

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

abundant :: Factors -> Bool
abundant = (\(x:xs) -> x < sum xs) . divisors

totient :: Factors -> Int
totient = IM.foldrWithKey (\k v acc -> acc * (k ^ (v - 1)) * (k - 1)) 1 . unFactors

coprimesTo :: Int -> Factors -> [Factors]
coprimesTo limit n = fmap toF . flip evalStateT (limit, avail) . many $ do
  (k, ps) <- get
  {- guard (not (null ps)) -}
  (p,ps') <- lift $ select ps
  expon <- lift [1 .. maxpower k p]
  let k' = k `div` (p ^ expon)
  guard (k' >= 1)
  put (k', ps')
  pure (p, expon)
    where
      avail = IS.toAscList $ IS.difference (IS.fromDistinctAscList (takeWhile (<= limit) primes)) (IS.fromList (IM.keys (unFactors n)))
      maxpower (fromIntegral->a) (fromIntegral->b) = floor @Double (logBase b a)
      toF = Factors . IM.fromAscList . filter ((/= 0) . snd)

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

go :: IO String
go = do
  pos <- getLine
  case head pos of
    'a' -> return pos
    _   -> go
