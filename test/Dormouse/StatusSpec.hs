module Dormouse.StatusSpec
  ( tests
  ) where

import Test.Hspec
import Dormouse.Status

tests :: IO()
tests = hspec $ 
  describe "Successful" $ do
    it "matches a status code of 200" $
      let wasSuccess = 
           case 200 of
             Successful -> True
             _ -> False in
      wasSuccess `shouldBe` True
    it "does not match a status code of 400" $
      let wasSuccess = 
           case 400 of
             Successful -> True
             _ -> False in
      wasSuccess `shouldBe` False
