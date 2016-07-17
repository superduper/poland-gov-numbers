{-# LANGUAGE OverloadedStrings #-}
module Data.Poland.KRS (
  KRS(..)
, randomKRS
) where

import System.Random
import Data.Maybe
import Data.Poland.Internal.Parse
import Text.Parsec

newtype KRS = KRS (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) deriving Eq

parseKRS :: String -> Either ParseError KRS
parseKRS = runParser krsParser () "KRS.parseKRS input"

krsParser = digitParser 10 toKRS
  where
    toKRS :: [Int] -> Either String KRS
    toKRS [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10] =
         Right $ KRS (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)
    toKRS _ = error "KRS.krsParser: should never reach this point, pattern matching failed"

instance Show KRS where
  show (KRS (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)) = mconcat $ show <$> [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10]

instance Read KRS where
  readsPrec _ s = either (const []) (\p -> [(p, "")]) (parseKRS s)

randomKRS :: IO KRS
randomKRS = KRS <$> gen
  where gen = (,,,,,,,,,) <$> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
        randN = randomRIO (0, 9)

