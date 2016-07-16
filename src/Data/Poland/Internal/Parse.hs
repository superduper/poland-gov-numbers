module Data.Poland.Internal.Parse (
  asDigit
, isDigit
, digitsToInt
) where

import Data.Maybe
import Data.Char

asDigit :: Char -> Maybe Int
asDigit c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | otherwise            = Nothing

-- | Converts a list of digits into Int
-- src: http://hackage.haskell.org/package/digits-0.1/docs/src/Data-Digits.html
digitsToInt :: [Int] -> Int
digitsToInt = foldl (\ a b -> a * 10 + b) 0
