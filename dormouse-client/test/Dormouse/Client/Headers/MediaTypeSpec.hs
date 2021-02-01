module Dormouse.Client.Headers.MediaTypeSpec
  ( spec
  ) where

import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as Map
import Dormouse.Client.Headers.MediaType
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMediaType" $ do
    it "parses application/json media type correctly" $ do
      mediaType <- parseMediaType "application/json"
      mediaType `shouldBe` applicationJson
    it "parses application/x-www-form-urlencoded" $ do
      mediaType <- parseMediaType "application/x-www-form-urlencoded"
      mediaType `shouldBe` applicationXWWWFormUrlEncoded
    it "parses text/html" $ do
      mediaType <- parseMediaType "text/html"
      mediaType `shouldBe` textHtml
  describe "mediaTypeAsByteString" $ do
    it "creates correct ByteString for applicationJson" $ do
      encodeMediaType applicationJson `shouldBe` "application/json"
    it "creates correct ByteString for applicationXWWWFormUrlEncoded" $ do
      encodeMediaType applicationXWWWFormUrlEncoded `shouldBe` "application/x-www-form-urlencoded"
    it "creates correct ByteString for textHtml" $ do
      encodeMediaType textHtml `shouldBe` "text/html"
  