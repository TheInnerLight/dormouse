{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Dormouse.Client.UrlReqSpec
  ( spec
  ) where

import Test.Hspec
import Dormouse.Url
import Dormouse.Url.QQ
import Dormouse.Client.MonadIOImpl
import qualified Network.HTTP.Client as C

spec :: Spec
spec = do
  describe "genClientRequestFromUrlComponents" $ do
    it "generates a non-secure request for an http uri" $ do
      let anyUrl = asAnyUrl [http|http://google.com|]
          req = genClientRequestFromUrlComponents anyUrl
      C.secure req `shouldBe` False
    it "generates a secure request for an https uri" $ do
      let anyUrl = asAnyUrl [https|https://google.com|]
          req = genClientRequestFromUrlComponents anyUrl
      C.secure req `shouldBe` True
    it "generates a non-secure request with the correct host" $ do
      let anyUrl = asAnyUrl [http|http://google.com|]
          req = genClientRequestFromUrlComponents anyUrl
      C.host req `shouldBe` "google.com"
    it "generates a request on port 80 for an http uri with no provided port" $ do
      let anyUrl = asAnyUrl [http|http://google.com|]
          req = genClientRequestFromUrlComponents anyUrl
      C.port req `shouldBe` 80
    it "generates a request on the supplied port for an http uri with a provided port" $ do
      let anyUrl = asAnyUrl [http|http://google.com:8080|]
          req = genClientRequestFromUrlComponents anyUrl
      C.port req `shouldBe` 8080
    it "generates a request on port 443 for an https uri with no provided port" $ do
      let anyUrl = asAnyUrl [https|https://google.com|]
          req = genClientRequestFromUrlComponents anyUrl
      C.port req `shouldBe` 443
    it "generates a request on the supplied port for an https uri with a provided port" $ do
      let anyUrl = asAnyUrl [https|https://google.com:8443|]
          req = genClientRequestFromUrlComponents anyUrl
      C.port req `shouldBe` 8443
    it "generates a request with the correct path (simple)" $ do
      let anyUrl = asAnyUrl [https|https://google.com/my/path/is/great|]
          req = genClientRequestFromUrlComponents anyUrl
      C.path req `shouldBe` "/my/path/is/great"
    it "generates a request with the correct path (unicode)" $ do
      let anyUrl = asAnyUrl [https|https://google.com/my/path/is/great%F0%9F%98%88|]
          req = genClientRequestFromUrlComponents anyUrl
      C.path req `shouldBe` "/my/path/is/great%F0%9F%98%88"
    it "generates a request with the correct query params (simple)" $ do
      let anyUrl = asAnyUrl [https|https://google.com?test=abc|]
          req = genClientRequestFromUrlComponents anyUrl
      C.queryString req `shouldBe` "?test=abc"
    it "generates a request with the correct query params (unicode)" $ do
      let anyUrl = asAnyUrl [https|https://google.com?test=%F0%9F%98%80|]
          req = genClientRequestFromUrlComponents anyUrl
      C.queryString req `shouldBe` "?test=%F0%9F%98%80"
