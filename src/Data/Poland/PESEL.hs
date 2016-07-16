{-# LANGUAGE OverloadedStrings  #-}
module Data.Poland.PESEL (
    PESEL(..)
  , Sex(..)
  , isValidPESEL
  , randomPESEL
  , peselDate
  , peselSex
) where

import System.Random
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Either
import Data.Time.Calendar

import Data.Poland.Internal.Parse

type PESELBase   = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

type PESELDate   = (Int, Int, Int, Int, Int, Int)
type PESELSerial = (Int, Int, Int, Int)

data PESEL = PESEL !PESELDate !PESELSerial !Int deriving (Eq)
data Sex = Male
         | Female deriving (Eq, Show)

instance Show PESEL where
  show (PESEL (n1, n2, n3, n4, n5, n6) (n7, n8, n9, n10) sum) = mconcat $ show <$> [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, sum]

instance Read PESEL where
  readsPrec _ = readP_to_S parsePESEL

parsePESEL :: ReadP PESEL
parsePESEL =
  do numbers <- munch isDigit
     case (fromJust . asDigit) <$> numbers of
       [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11] -> do
         let pesel = PESEL (n1, n2, n3, n4, n5, n6) (n7, n8, n9, n10) n11
         if isValidPESEL pesel then return pesel
         else error "Invalid PESEL: checksum doesn't match"
       otherwise -> error $ "Invalid PESEL: invalid format, consumed character count: " ++ (show $ length otherwise)

peselBase :: PESEL -> PESELBase
peselBase (PESEL (n1, n2, n3, n4, n5, n6) (n7, n8, n9, n10) _) = (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)

mkPeselBase :: PESELDate -> PESELSerial -> PESELBase
mkPeselBase (n1, n2, n3, n4, n5, n6) (n7, n8, n9, n10) = (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)

-- | PESEL checksum calculation
--
-- Jedenasta cyfra jest cyfrą kontrolną, służącą do wychwytywania przekłamań numeru. Jest ona generowana na podstawie pierwszych dziesięciu cyfr. Aby sprawdzić czy dany numer PESEL jest prawidłowy, należy, zakładając, że litery a-j to kolejne cyfry numeru od lewej, obliczyć wyrażenie:
-- 1×a + 3×b + 7×c + 9×d + 1×e + 3×f + 7×g + 9×h + 1×i + 3×j
-- Następnie należy odjąć ostatnią cyfrę otrzymanego wyniku od 10. Jeśli otrzymany wynik nie jest równy cyfrze kontrolnej, to znaczy, że numer zawiera błąd[15].
--
-- Uwaga implementacyjna – jeśli ostatnią cyfrą otrzymanego wyniku jest 0, w wyniku odejmowania otrzymamy liczbę 10, podczas gdy suma kontrolna jest cyfrą. Oznacza to tyle, że cyfra kontrolna winna być równa 0 (stąd dobrze jest wykonać na wyniku odejmowania operację modulo 10). W wyniku niezbyt precyzyjnego opisu na stronie MSW ten aspekt jest często pomijany i prowadzi do błędów w implementacji sprawdzania poprawności numeru PESEL.
--
-- Przykład dla numeru PESEL 44051401358:
--
-- 1*4 + 3*4 + 7*0 + 9*5 + 1*1 + 3*4 + 7*0 + 9*1 + 1*3 + 3*5 = 101
-- Wyznaczamy resztę z dzielenia sumy przez 10:
--
-- 101:10 = 10 reszta = 1
-- Jeżeli reszta = 0, to cyfra kontrolna wynosi 0. Jeżeli reszta ≠ 0, to cyfra kontrolna będzie uzupełnieniem reszty do 10, czyli w podanym przykładzie jest to cyfra 9.
-- 10 – 1 = 9
checksumPESELBase :: PESELBase -> Int
checksumPESELBase (n1, n2, n3, n4, n5, n6, n7, n8, n9, n10) = if modulo == 0 then 0 else 10 - modulo
  where n1'  = n1  * 1
        n2'  = n2  * 3
        n3'  = n3  * 7
        n4'  = n4  * 9
        n5'  = n5  * 1
        n6'  = n6  * 3
        n7'  = n7  * 7
        n8'  = n8  * 9
        n9'  = n9  * 1
        n10' = n10 * 3
        sum  = n1' + n2' + n3' + n4' + n5' + n6' + n7' + n8' + n9' + n10'
        modulo = sum `mod` 10

data PeselCentury = Century1800
                  | Century1900
                  | Century2000
                  | Century2100
                  | Century2200

-- | Map first digit form month to a century
toCentury :: Int -> PeselCentury
toCentury n
 | n == 8 || n == 9 = Century1800
 | n == 0 || n == 1 = Century1900
 | n == 2 || n == 3 = Century2000
 | n == 4 || n == 5 = Century2100
 | n == 6 || n == 7 = Century2200
 | otherwise = error "PESEL.toCentury: unable to decode century from first digit of month"

yearToPeselCentury :: Int -> PeselCentury
yearToPeselCentury year
 | year >= 1800 && year <= 1900 = Century1800
 | year >= 1900 && year <= 2000 = Century1900
 | year >= 2000 && year <= 2100 = Century2000
 | year >= 2100 && year <= 2200 = Century2100
 | year >= 2200 && year <= 2300 = Century2200
 | otherwise = error "PESEL.yearToPeselCentury: year is out of 1800-2300 range"

centuryMonthOffset :: PeselCentury -> Int
centuryMonthOffset Century1800 = 80
centuryMonthOffset Century1900 = 0
centuryMonthOffset Century2000 = 20
centuryMonthOffset Century2100 = 40
centuryMonthOffset Century2200 = 60

-- | Maps pesel date into a gregorian calendar day
peselDateToDay :: PESELDate -> Either String Day
peselDateToDay d@(n1, n2, n3, n4, n5 ,n6) =
  let cent = toCentury n3
      centStarts = case cent of
                    Century1800 -> 1800
                    Century1900 -> 1900
                    Century2000 -> 2000
                    Century2100 -> 2100
                    Century2200 -> 2200
      monthSubtract = centuryMonthOffset cent
      year = fromIntegral $ centStarts + digitsToInt [n1, n2]
      month = digitsToInt [n3, n4] - monthSubtract
      day = digitsToInt [n5, n6]
  in  maybe (Left $ "Invalid date: " ++ show year ++ "-" ++ show month ++ "-" ++ show day ++ ":: d is" ++ show d) Right (fromGregorianValid year month day)


-- | Extracts date of birth from PESEL
peselBaseToDate :: PESELBase -> Either String Day
peselBaseToDate base@(n1, n2, n3, n4, n5, n6, _, _, _, _) = peselDateToDay (n1, n2, n3, n4, n5 ,n6)

-- | Płeć
--  Informacja o płci osoby, której zestaw informacji jest identyfikowany, zawarta jest na 10. (przedostatniej) pozycji numeru PESEL.
-- cyfry 0, 2, 4, 6, 8 – oznaczają płeć żeńską
-- cyfry 1, 3, 5, 7, 9 – oznaczają płeć męską
-- Po zmianie płci przydzielany jest nowy numer PESEL[14].
peselBaseToSex :: PESELSerial -> Sex
peselBaseToSex (_, _, _, n10) = if n10 `mod` 2 == 0 then Female else Male

peselSex :: PESEL -> Sex
peselSex (PESEL _ serial _) = peselBaseToSex serial

peselDate :: PESEL -> Day
peselDate (PESEL date _ _) = either error id $ peselDateToDay date

isValidPESELBase :: PESELBase -> Bool
isValidPESELBase = isRight . peselBaseToDate

isValidPESEL :: PESEL -> Bool
isValidPESEL p@(PESEL _ _ n11) =
  let base = peselBase p
      baseOk = isValidPESELBase base
      sumOk = n11' == n11
      n11' = checksumPESELBase base
  in baseOk && sumOk

genPeselRangedDate :: IO Day
genPeselRangedDate =
  let genDay year = if isLeapYear year then randomRIO (0, 365) :: IO Integer
                    else randomRIO (0, 364) :: IO Integer
  in do year <- randomRIO(1800,2200) :: IO Integer
        day <- genDay year
        let firstDay = fromGregorian year 1 1
        return $ addDays day firstDay

toDigits :: Integral a => a -> (a, a)
toDigits x = (x `div` 10, x `mod` 10)

integerToDigits :: Integer -> (Int, Int)
integerToDigits = (bimap toInt) . toDigits

intToDigits :: Int -> (Int, Int)
intToDigits = toDigits

toInt = fromIntegral :: Integral a => a -> Int
bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (x, y) = (f x, f y)

dayToPeselDate :: Day -> PESELDate
dayToPeselDate date =
  let (year, month, day) = toGregorian date
      monthOffset :: Int
      monthOffset = centuryMonthOffset $ yearToPeselCentury $ fromIntegral $ year
      (y1, y2) = integerToDigits $ year `mod` 100
      (m1, m2) = intToDigits $ monthOffset + month
      (d1, d2) = intToDigits $ day
  in (y1, y2, m1, m2, d1, d2)

randomPESEL = go
  where randN   = randomRIO (0, 9)
        genSerial = (,,,) <$> randN
                          <*> randN
                          <*> randN
                          <*> randN
        genDate = dayToPeselDate <$> genPeselRangedDate
        go = do date <- genDate
                serial <- genSerial
                let base = mkPeselBase date serial
                return $ PESEL date serial $ checksumPESELBase base


