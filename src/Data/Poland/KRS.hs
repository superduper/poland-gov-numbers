{-# LANGUAGE OverloadedStrings #-}
module Data.Poland.KRS (
  KRS(..)
, randomKRS
) where

import System.Random
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Poland.Internal.Parse

newtype KRS = KRS (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) deriving Eq

parseKRS :: ReadP KRS
parseKRS =
  do numbers <- munch isDigit
     case (fromJust . asDigit) <$> numbers of
       [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10] -> return $
         KRS (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)
       otherwise -> error $ "Invalid KRS: invalid format, consumed character count: " ++ (show $ length otherwise)

instance Show KRS where
  show (KRS (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)) = mconcat $ show <$> [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10]

instance Read KRS where
  readsPrec _ = readP_to_S parseKRS

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

