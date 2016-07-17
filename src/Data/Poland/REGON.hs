{-# LANGUAGE OverloadedStrings #-}
module Data.Poland.REGON (
    REGON(..)
  , REGON9Base
  , REGON14Base
  , isValidREGON
  , checksumREGON9Base
  , checksumREGON14Base
  , randomREGON
  , randomREGON9
  , randomREGON14
  , parseREGON
  , regonParser
) where

import System.Random
import Data.Maybe
import Data.Poland.Internal.Parse
import Text.Parsec
import Text.Parsec.Char
import Control.Monad

type REGON9Base = (Int, Int, Int, Int, Int, Int, Int, Int)
type REGON14Base = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

data REGON = REGON9  !REGON9Base !Int
           | REGON14 !REGON14Base !Int
           deriving Eq

instance Show REGON where
  show (REGON9 (n1, n2, n3, n4, n5, n6, n7, n8) sum) =
    mconcat $ show <$> [n1, n2, n3, n4, n5, n6, n7, n8, sum]
  show (REGON14 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) sum) =
    mconcat $ show <$> [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, sum]

instance Read REGON where
  readsPrec _ s = either (const []) (\p -> [(p, "")]) (parseREGON s)

parseREGON :: String -> Either ParseError REGON
parseREGON = runParser regonParser () "REGON.parseREGON input"


regonParser :: Monad m => ParsecT String () m REGON
regonParser = do
  rawDigits <- countFromTo 9 14 digit
  let intDigits = (fromJust . charDigitAsInt) <$> rawDigits
  either parserFail return (toREGON intDigits)
  where
    toREGON :: [Int] -> Either String REGON
    toREGON [n1, n2, n3, n4, n5, n6, n7, n8, n9] =
         let reg = REGON9 (n1, n2, n3, n4, n5, n6, n7, n8) n9
         in if isValidREGON reg then return reg
            else Left "Invalid REGON9: checksum doesn't match"
    toREGON [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, sum] =
         let reg = REGON14 (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) sum
         in if isValidREGON reg then return reg
            else Left "Invalid REGON14: checksum doesn't match"
    toREGON _ = error "REGON.REGONParser: should never reach this point, pattern matching failed"


-- | REGON checksum calculation
--
--  Do sprawdzenia poprawności numeru REGON należy wykonać następujące kroki :
--    pierwsze 8 cyfr numeru należy pomnożyć kolejno przez cyfry 8, 9, 2, 3, 4, 5, 6 i 7,
--    a następnie wynikowy iloczyn podzielić modulo przez 11.
-- Jeśli otrzymana wartość równa jest ostatniej cyfrze numeru oznacza to, że REGON jest poprawny.
-- W przypadku, gdy mod zwraca 10 za cyfrę kontrolną przyjmuje się 0.
checksumREGON9Base :: REGON9Base -> Int
checksumREGON9Base (n1, n2, n3, n4, n5, n6, n7, n8) =
  if modulo == 10 then 0 else modulo
  where n1'  = n1 * 8
        n2'  = n2 * 9
        n3'  = n3 * 2
        n4'  = n4 * 3
        n5'  = n5 * 4
        n6'  = n6 * 5
        n7'  = n7 * 6
        n8'  = n8 * 7
        sum  = n1' + n2' + n3' + n4' + n5' + n6' + n7' + n8'
        modulo = sum `mod` 11

-- | REGON14
-- Jednostkom organizacyjnym, które mają swoje siedziby w różnych województwach nadaje się REGON 14-cyfrowy (identyfikator jednostki lokalnej). Dziewięć pierwszych cyfr takiego numeru pokrywa się z REGON-em jednostki macierzystej. Cyfra kontrolna numeru obliczana jest tym samym sposobem, ale wagi są inne. Ponieważ jeden ze współczynników wagowych wynosi zero to weryfikacja „długiego” REGON-u wymaga weryfikacji 9 cyfr z wagami jak dla 9-cyfrowego numeru, a następnie weryfikacji długiego REGON-u z innymi wagami.
-- Suma kontrolna
-- W przypadku REGON-ów 14-cyfrowych cyfrę kontrolną oblicza się następująco:
-- Wagi: 2 4 8 5 0 9 7 3 6 1 2 4 8
-- Pierwsze trzynaście cyfr REGON: 1 2 3 4 5 6 7 8 5 1 2 3 4
-- Wynik: 1*2 + 2*4 + 3*8 + 4*5 + 5*0 + 6*9 + 7*7 + 8*3 + 5*6 + 1*1 + 2*2 + 3*4 + 4*8 = 260
-- Cyfra kontrolna: 260 mod 11 = 7
-- Cały REGON: 12345678512347
-- W przypadku, gdy mod zwraca 10 za cyfrę kontrolną przyjmuje się 0.
checksumREGON14Base :: REGON14Base -> Int
checksumREGON14Base (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13) =
  if modulo == 10 then 0 else modulo
  where n1'  = n1  * 2
        n2'  = n2  * 4
        n3'  = n3  * 8
        n4'  = n4  * 5
        n5'  = n5  * 0
        n6'  = n6  * 9
        n7'  = n7  * 7
        n8'  = n8  * 3
        n9'  = n9  * 6
        n10' = n10 * 1
        n11' = n11 * 2
        n12' = n12 * 4
        n13' = n13 * 8
        sum  = n1' + n2' + n3' + n4' + n5' + n6' + n7' + n8' + n9' + n10' + n11' + n12' + n13'
        modulo = sum `mod` 11


isValidREGON :: REGON -> Bool
isValidREGON (REGON9  base n9)  = checksumREGON9Base base == n9
isValidREGON (REGON14 base n14) = checksumREGON14Base base == n14

randN   = randomRIO (0, 9)

randomREGON9 :: IO REGON
randomREGON9 =
  let genBase = (,,,,,,,) <$> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
                          <*> randN
  in do base <- genBase
        return $ REGON9 base $ checksumREGON9Base base

randomREGON14 :: IO REGON
randomREGON14 =
  let genBase = (,,,,,,,,,,,,) <$> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
                               <*> randN
  in do base <- genBase
        return $ REGON14 base $ checksumREGON14Base base

randomREGON :: IO REGON
randomREGON = join $ (generators !!) <$> randomRIO (0, 1)
  where generators = [randomREGON9, randomREGON14]
