import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mapM)
import Control.Exception (evaluate)

import Data.Poland.KRS   (randomKRS, KRS(..))
import Data.Poland.NIP   (randomNIP, NIP(..))
import Data.Poland.PESEL (randomPESEL, PESEL(..), peselSex, Sex(..), peselDate)
import Data.Poland.REGON (REGON(..), randomREGON9, randomREGON14)
import Data.Time.Calendar


main :: IO ()
main = hspec $
  describe "Registry Numbers Tests" $ do
    context "KRS" $ do
      let asKRS = read :: String -> KRS
      prop "read . show is identity" $ monadicIO $ do
        krs <- liftIO randomKRS
        assert $ krs == (read $ show krs)

      it "fails parse on invalid numbers" $ do
          let ks = [ "874r3617r"
                   , "950r2738r"
                   , "190r5216r"
                   , "012r9263r"]
          let check r = evaluate (asKRS r) `shouldThrow` errorCall "Invalid KRS: invalid format, consumed character count: 3"
          mapM_ check ks

      it "parses valid numbers" $ do
          let ks = [ "1234567890"
                   , "0234567890"
                   , "0034567890"
                   , "0204567890"]
          let check k = k `shouldBe` (show $ asKRS k)
          mapM_ check ks

    context "REGON" $ do
      let asRegon = read :: String -> REGON
      prop "read . show is identity for regon9" $ monadicIO $ do
        regon9 <- liftIO randomREGON9
        assert $ regon9 == (read $ show regon9)

      prop "read . show is identity for regon14" $ monadicIO $ do
        regon14 <- liftIO randomREGON14
        assert $ regon14 == (read $ show regon14)

      it "parses valid numbers" $ do
          let rs = [ "874536175"
                   , "950427384"
                   , "190852164"
                   , "012192639"]
          let check r = r `shouldBe` (show $ asRegon r)
          mapM_ check rs

      it "parse fails on invalid format" $ do
          let rs = [ "87453617r"
                   , "95042738r"
                   , "19085216r"
                   , "01219263r"]
          let check r = evaluate (asRegon r) `shouldThrow` errorCall "Invalid REGON: invalid format, consumed character count: 8"
          mapM_ check rs

      it "parse fails on invalid checksums" $ do
          let rs9 = [ "874536170"
                    , "950427380"
                    , "190852160"
                    , "012192630"]
          let check r = evaluate (asRegon r) `shouldThrow` errorCall "Invalid REGON9: checksum doesn't match"
          mapM_ check rs9

    context "NIP" $ do
      let asNIP = read :: String -> NIP
      prop "read . show is identity" $ monadicIO $ do
        nip <- liftIO randomNIP
        assert $ nip == (read $ show nip)

      it "parse fails on invalid checksums" $ do
          let ns = [ "2699361272"
                   , "1937155290"
                   , "6292472216"
                   , "9948541799"]
          let check n = n `shouldBe` (show $ asNIP n)
          mapM_ check ns

      it "parse fails on invalid checksums" $ do
          let ns = [ "2699361202"
                   , "1937155200"
                   , "6292472206"
                   , "9948541709"]
          let check n = evaluate (asNIP n) `shouldThrow` errorCall "Invalid NIP: checksum doesn't match"
          mapM_ check ns

    context "PESEL" $ do
      let asPESEL = read :: String -> PESEL
      prop "read . show is identity" $ monadicIO $ do
        pesel <- liftIO randomPESEL
        assert $ pesel == (read $ show pesel)

      it "detects male gender" $
          peselSex (read "39082411473") `shouldBe` Male

      it "detects female gender" $
          peselSex (read "51322564664") `shouldBe` Female

      it "converts date from PESEL correctly" $ do
          let ps = [ ("00892232907", (1800, 9, 22))
                   , ("39082411473", (1939, 8, 24))
                   , ("13321615659", (2013, 12, 16))
                   , ("51322564664", (2051, 12, 25))
                   , ("16442644010", (2116, 4, 26)) ]
          let check (p, (y,m,d)) = (peselDate $ read p) `shouldBe` (fromGregorian y m d)
          mapM_ check ps

      it "parse is ok on valid checksums" $ do
          let ps = [ "39082411473"
                   , "82020900957"
                   , "63021904836"
                   , "13321615659"]
          let check p = p `shouldBe` (show $ asPESEL p)
          mapM_ check ps

      it "parse fails on invalid checksums" $ do
          let ps = [ "39082411472"
                   , "82020900952"
                   , "63021904832"
                   , "13321615652"]
          let check p = evaluate (asPESEL p) `shouldThrow` errorCall "Invalid PESEL: checksum doesn't match"
          mapM_ check ps
