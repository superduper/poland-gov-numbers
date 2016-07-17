module Data.Poland.Internal.Parse (
    charDigitAsInt
  , digitsToInt
  , digitParser
  , countUpTo
  , countFromTo
) where

import Data.Maybe
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Control.Monad

charDigitAsInt :: Char -> Maybe Int
charDigitAsInt c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | otherwise            = Nothing

-- | Converts a list of digits into Int
-- src: http://hackage.haskell.org/package/digits-0.1/docs/src/Data-Digits.html
digitsToInt :: [Int] -> Int
digitsToInt = foldl (\ a b -> a * 10 + b) 0

digitParser :: Monad m
             => Int
             -> ([Int] -> Either String a)
             -> ParsecT String () m a
digitParser ns parse =
  do rawDigits <- count ns digit
     let intDigits = (fromJust . charDigitAsInt) <$> rawDigits
     either parserFail return (parse intDigits)

-- | @countUpto n p@ parses zero to @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of up to
-- @n@ values returned by @p@.
countUpTo :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
countUpTo n p
    | n <= 0 = return []
    | otherwise = option [] (liftM2 (:) p (countUpTo (pred n) p))

-- | @countUpto m n p@ parses @m@ to @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of @m@ to
-- @n@ values returned by @p@.
countFromTo :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countFromTo l r p
    | r < l = return []
    | otherwise = liftM2 (++) (count l p) (countUpTo (r - l) p)
