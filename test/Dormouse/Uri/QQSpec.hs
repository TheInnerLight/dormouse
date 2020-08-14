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
      let uri = [http|http://google.com|]
      let matched = case uri of
            [http|http://google.com|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern matche doesn't match a different uri" $ do
      let uri = [http|http://google.com|]
      let matched = case uri of
            [http|http://test.com|] -> True
            _ -> False
      matched `shouldBe` False
  describe "https pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let uri = [https|https://google.com|]
      let matched = case uri of
            [https|https://google.com|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let uri = [https|https://google.com|]
      let matched = case uri of
            [https|https://test.com|] -> True
            _ -> False
      matched `shouldBe` False
  describe "absoluteUri pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let uri = [absoluteUri|https://google.com|]
      let matched = case uri of
            [absoluteUri|https://google.com|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let uri = [absoluteUri|https://google.com|]
      let matched = case uri of
            [absoluteUri|https://test.com|] -> True
            _ -> False
      matched `shouldBe` False
  describe "relativeUri pattern" $ do
    it "pattern matches correctly against a matching uri" $ do
      let uri = [relativeUri|/myPath|]
      let matched = case uri of
            [relativeUri|/myPath|] -> True
            _ -> False
      matched `shouldBe` True
    it "pattern match doesn't match a different uri" $ do
      let uri = [relativeUri|/myPath2|]
      let matched = case uri of
            [relativeUri|/myPath|] -> True
            _ -> False
      matched `shouldBe` False
