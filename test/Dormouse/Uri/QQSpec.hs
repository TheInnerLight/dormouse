{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Dormouse.Uri.QQSpec 
  ( tests
  ) where

import Test.Hspec
import Dormouse.Uri
import Dormouse.Uri.QQ

tests :: IO()
tests = hspec $ do
  describe "http pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let url = [http|http://google.com|]
      let matched = case url of
            [http|http://google.com|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let url = [http|http://google.com|]
      let matched = case url of
            [http|http://test.com|] -> True
            _ -> False
      matched `shouldBe` False
  describe "https pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let url = [https|https://google.com|]
      let matched = case url of
            [https|https://google.com|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let url = [https|https://google.com|]
      let matched = case url of
            [https|https://test.com|] -> True
            _ -> False
      matched `shouldBe` False
  describe "uri pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let x = [uri|https://google.com|]
      let matched = case x of
            [uri|https://google.com|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let x = [uri|https://google.com|]
      let matched = case x of
            [uri|https://test.com|] -> True
            _ -> False
      matched `shouldBe` False
  describe "relativeUri pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let x = [uri|/myPath|]
      let matched = case x of
            [uri|/myPath|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let x = [uri|/myPath2|]
      let matched = case x of
            [uri|/myPath|] -> True
            _ -> False
      matched `shouldBe` False
