module Euler.P010.Problem017
  ( prob017
  )
  where

{-
 -If the numbers 1 to 5 are written out in words: one, two, three, four, five,
 -then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 -
 -If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
 -how many letters would be used?
 -
 -
 -NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
 -contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
 -The use of "and" when writing out numbers is in compliance with British usage.
 -}

import qualified Data.Text as T
import           Data.Char                 (isAlpha)
import           Data.Maybe                (catMaybes)
import           Text.Numeral              (defaultInflection)
import           Text.Numeral.Language.ENG (gb_cardinal)

prob017 :: Integer
prob017 = prob017' 1000

-- naive method

-- Numerals has a bug where the "gb_cardinal" does not properly place the "and"
-- in numerals > 100, due to
-- https://github.com/roelvandijk/numerals/blame/bcc7f8ac65876bfe3be75ea24cdb3b0e15a5102e/src/Text/Numeral/Language/ENG.hs#L135
prob017' :: Integer -> Integer
prob017' limit = fromIntegral $ ands + ((T.length . T.filter isAlpha . T.unwords . catMaybes
                                       . fmap (gb_cardinal defaultInflection) . enumFromTo 1) limit)

  where
    ands = length . concatMap (const "and") . filter (\x -> x > 100 && (x `mod` 100) /= 0)
           $ enumFromTo 100 limit
