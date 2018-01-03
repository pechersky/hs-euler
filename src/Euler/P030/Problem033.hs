module Euler.P030.Problem033
  ( prob033
  )
  where

{- The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
 -
 - We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 -
 - There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
 -
 - If the product of these four fractions is given in its lowest common terms, find the value of the denominator. -}

import qualified Data.Set as S
import           Data.Ratio ((%), denominator)
import           Data.List  (nub)

prob033 :: Integer
prob033 = prob033' 2

-- naive method

prob033' :: Integer -> Integer
prob033' limit = denominator . product $ [a % b | a <- avail, b <- avail , a < b
                                                , b `mod` base /= 0, match a b]
  where
    base = 10 ^ (limit - 1)
    avail = [base .. 10 * base - 1]
    match a b = S.size (shared a b) == 1 && nub (show a) == show a && nub (show b) == show b
                && shared a b /= S.singleton '0' && (s a b % s b a) == (a % b)
    s :: Integer -> Integer -> Integer
    s a b = read . S.toList . flip S.difference (shared a b). S.fromList . show $ a
    shared a b = S.intersection (S.fromList (show a)) (S.fromList (show b))
