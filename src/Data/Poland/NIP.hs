{-# LANGUAGE OverloadedStrings   #-}
module Data.Poland.NIP (
  NIP(..)
, NIPBase
, isValidNIPBase
, isValidNIP
, checksumNIPBase
, randomNIP
) where

import System.Random
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Poland.Internal.Parse

type NIPBase = (Int, Int, Int, Int, Int, Int, Int, Int, Int)
data NIP = NIP !NIPBase !Int deriving Eq

instance Show NIP where
  show (NIP (n1, n2, n3, n4, n5, n6, n7, n8, n9) sum) = mconcat $ show <$> [n1, n2, n3, n4, n5, n6, n7, n8, n9, sum]

instance Read NIP where
  readsPrec _ = readP_to_S parseNIP

parseNIP :: ReadP NIP
parseNIP =
  do numbers <- munch isDigit
     case (fromJust . asDigit) <$> numbers of
       [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10] -> do
         let nip = NIP (n1, n2, n3, n4, n5, n6, n7, n8, n9) n10
         if isValidNIP nip then return nip
         else error "Invalid NIP: checksum doesn't match"
       otherwise -> error $ "Invalid NIP: invalid format, consumed character count: " ++ (show $ length otherwise)

-- | NIP checksum calculation
--
-- Dziesiąta cyfra NIP jest cyfrą kontrolną, obliczaną według poniższego algorytmu:
--   Pomnożyć każdą z pierwszych dziewięciu cyfr odpowiednio przez wagi: 6, 5, 7, 2, 3, 4, 5, 6, 7.
--   Zsumować wyniki mnożenia.
--   Obliczyć resztę z dzielenia przez 11 (operacja modulo 11).
--   NIP jest tak generowany, aby nigdy w wyniku tego dzielenia nie wyszła liczba 10.
--   Zgodnie z tym algorytmem numer 000-000-00-00 jest prawidłowy, ale nie ma sensu.
--   Dla ciągu cyfr 123-456-78-90 nie można dobrać cyfry kontrolnej by wygenerować prawidłowy NIP.

checksumNIPBase :: NIPBase -> Int
checksumNIPBase (n1, n2, n3, n4, n5, n6, n7, n8, n9) = sum `mod` 11
  where n1'  = n1 * 6
        n2'  = n2 * 5
        n3'  = n3 * 7
        n4'  = n4 * 2
        n5'  = n5 * 3
        n6'  = n6 * 4
        n7'  = n7 * 5
        n8'  = n8 * 6
        n9'  = n9 * 7
        sum  = n1' + n2' + n3' + n4' + n5' + n6' + n7' + n8' + n9'

isValidNIPBase :: NIPBase -> Bool
isValidNIPBase base = checksumNIPBase base /= 10

isValidNIP :: NIP -> Bool
isValidNIP (NIP base n10) = isValidNIPBase base && n10' == n10
  where n10' = checksumNIPBase base

randomNIP = go
  where randN   = randomRIO (0, 9)
        genBase = (,,,,,,,,) <$> randN
                             <*> randN
                             <*> randN
                             <*> randN
                             <*> randN
                             <*> randN
                             <*> randN
                             <*> randN
                             <*> randN
        go = do base <- genBase
                if isValidNIPBase base then return $ NIP base $ checksumNIPBase base
                else go

