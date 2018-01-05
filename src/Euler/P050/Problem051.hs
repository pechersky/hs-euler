module Euler.P050.Problem051
  ( prob051
  )
  where

{-
 - By replacing the 1st digit of the 2-digit number *3,
 - it turns out that six of the nine possible values:
 - 13, 23, 43, 53, 73, and 83, are all prime.
 -
 - By replacing the 3rd and 4th digits of 56**3 with the same digit,
 - this 5-digit number is the first example having seven primes
 - among the ten generated numbers, yielding the family:
 - 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
 - Consequently 56003, being the first member of this family,
 - is the smallest prime with this property.
 -
 - Find the smallest prime which, by replacing part of the number
 - (not necessarily adjacent digits) with the same digit,
 - is part of an eight prime value family.
 -}

import qualified Data.Sequence       as Seq
import           Data.Numbers.Primes (primes, isPrime)
import           Data.Digits         (digits, unDigits)
import           Data.Foldable       (toList)
import           Euler.Factors       (powerset)

prob051 :: Integer
prob051 = prob051' 8

-- naive method

prob051' :: Integer -> Integer
prob051' (fromIntegral->limit) = head . head . filter ((== limit) . length) . concatMap p . dropWhile (< 10) $ primes
  where
    p x = fmap (filter isPrime) $ variants
      where
        d = Seq.fromList . digits 10 $ x
        poslist = filter (/= []) . powerset $ [0..length d - 2]
        variants = fmap (fmap (unDigits 10 . toList) . f d) poslist
        f ds ps
          | 0 `elem` ps = f' [1..9]
          | otherwise   = f' [0..9]
          where
            f' ns = ($ ds) <$> (sequence . fmap r) ns ps
            r n = foldr1 (.) . fmap (($ n) . Seq.update)
