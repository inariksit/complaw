{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

-- DO NOT EDIT THIS FILE!
-- direct edits will be clobbered.
-- 
-- this file is autogenerated by tangling ex-20200702-safe-post/README.org
-- open the README.org in emacs and hit C-c C-v t to regenerate this file.

module Main where

import Test.Hspec
import SAFE.Basic
import Data.Ratio
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  forM_ [spec1] $ hspec
  return ()

safe_a = SAFE { owner="Investor A", money_in=200000, discount=Nothing, val_cap=(Just 4000000) }
safe_b = SAFE         "Investor B"           800000           Nothing          (Just 8000000)
series_a = EquityRound { valuationPre = 15000000
                       , new_money_in =  5000000
                       , commonPre    =  9250000
                       , optionsPreOutstanding = 300000
                       , optionsPrePromised    = 350000
                       , optionsPreFree = 100000
                       , optionsPost  = 10 / 100
                       , convertibles = [safe_a, safe_b]
                       , incoming     = [seriesA_c, seriesA_b, seriesA_other]
                       }

seriesA_c     = Equity { owner="Investor C", money_in=4000000,   shareClass="A" }
seriesA_b     = Equity { owner="Investor B", money_in=499998.97, shareClass="A" }
seriesA_other = Equity "Other New Investors"          500001.19             "A"
exit10 = LiquidityEvent { liquidityPrice = 10000000
                        , common         =  9250000
                        , optionsUsed    =   300000
                        , optionsFree    =   450000
                        , convertibles   = [safe_a, safe_b]
                        }

spec1 :: Spec
spec1 = do
    describe "estimatedDilution" $ do
      it "should compute the expected minimum dilution from II.1 as 15%" $
        estimatedDilution [safe_a, safe_b] `shouldBe` 15 / 100
    describe "dilutionDueTo" $ do
      it "investor A should be 5%" $
        dilutionDueTo (series_a.valuationPre) safe_a `shouldBe` 0.05
      it "investor B should be 10%" $
        dilutionDueTo (series_a.valuationPre) safe_b `shouldBe` 0.10
    describe "sharesPre" $ do
      it "should show total common + option shares as 10,000,000" $
        sharesPre series_a `shouldBe` 10000000
    describe "companyCapitalization" $ do
      it "should be 11,764,705" $
        companyCapitalization series_a `shouldBe` 11764705
    describe "conversionSharesAll" $ do
      it "conversion shares should add up to 1,764,705" $
        conversionSharesAll series_a `shouldBe` 1764705
    describe "conversionDilutions" $ do
      it "add up to 15% in this case" $
        conversionDilutions series_a `shouldBe` 0.15
    describe "conversionShares" $ do
      it "investor A's SAFE converts to   588,235 shares" $ conversionShares series_a safe_a `shouldBe`  588235
      it "investor B's SAFE converts to 1,176,470 shares" $ conversionShares series_a safe_b `shouldBe` 1176470
    describe "pricePerShare" $ do
      it "each Series A share should be priced at $1.1144" $
        pricePerShare series_a `shouldBe` 1.1144
    describe "totalPost" $ do
      it "for example 1, rounds to 17,946,424" $ totalPost series_a `shouldBe` 17946424
    describe "totalPost'" $ do
      it "for example 1, more precisely, should be 17,945,700 " $ totalPost' series_a `shouldBe` 17945700
    describe "optionsNewFree'" $ do
      it "if we were being precise we would issue 1,694,570 new options to arrive at a new pool sized at 10%" $ optionsNewFree' series_a `shouldBe` 1694570
    describe "optionsNewFree" $ do
      it "if we're rounding to the nearest thousand, we would issue 1,695,000 new options to arrive at a new pool sized at 10%" $ optionsNewFree series_a `shouldBe` 1695000
    describe "investorIssue" $ do
      it "investor C gets 3,589,375 shares for $4,000,000 "    $ investorIssue series_a seriesA_c `shouldBe` 3589375
      it "investor B gets   448,671 shares for $  499,998.97 " $ investorIssue series_a seriesA_b `shouldBe`  448671
      it "the others get    448,673 shares"                    $ investorIssue series_a seriesA_other `shouldBe`  448673
    describe "allInvestorIssues" $ do
      it "together, the new money turns into 4,486,719 shares " $ allInvestorIssues series_a `shouldBe` 4486719
    describe "optionsNewFree" $ do
      it "should add with optionsPreFree to make 1,795,000" $
        optionsNewFree series_a + optionsPreFree series_a `shouldBe` 1795000
    describe "optionsPost" $ do
      it "should come out to 10.00% " $
        floor(1000 * fromIntegral(optionsNewFree series_a + optionsPreFree series_a) / fromIntegral(totalPost series_a)) `shouldBe` floor(optionsPost series_a * 1000)
  --  describe "conversionAmount" $ do
  --    it "investor A's Conversion Amount is   561,764 shares" $ conversionAmountShares exit10 safe_a `shouldBe`  561764
  --    it "investor B's Conversion Amount is 1,123,527 shares" $ conversionAmountShares exit10 safe_b `shouldBe` 1123529
  --  describe "exitPricePerShare" $ do
  --    it "should be $0.8901" $ exitPricePerShare exit10 `shouldBe` 0.8901
