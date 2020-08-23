{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Dormouse.UrlSpec
  ( tests
  ) where

import Test.Hspec
import Dormouse.Url
import Dormouse.Url.QQ
import Dormouse.MonadIOImpl
import qualified Network.HTTP.Client as C

tests :: IO()
tests = hspec $ 
  describe "parseURI" $ do
    it "generates a non-secure request for an http uri" $ do
      let (scheme, comps) = createRequest [http|http://google.com|]
          req = genClientRequestFromUrlComponents scheme comps
      C.secure req `shouldBe` False
    it "generates a secure request for an https uri" $ do
      let (scheme, comps) = createRequest [https|https://google.com|]
          req = genClientRequestFromUrlComponents scheme comps
      C.secure req `shouldBe` True
    it "generates a non-secure request with the correct host" $ do
      let (scheme, comps) = createRequest [http|http://google.com|]
          req = genClientRequestFromUrlComponents scheme comps
      C.host req `shouldBe` "google.com"
    it "generates a request on port 80 for an http uri with no provided port" $ do
      let (scheme, comps) = createRequest [http|http://google.com|]
          req = genClientRequestFromUrlComponents scheme comps
      C.port req `shouldBe` 80
    it "generates a request on the supplied port for an http uri with a provided port" $ do
      let (scheme, comps) = createRequest [http|http://google.com:8080|]
          req = genClientRequestFromUrlComponents scheme comps
      C.port req `shouldBe` 8080
    it "generates a request on port 443 for an https uri with no provided port" $ do
      let (scheme, comps) = createRequest [https|https://google.com|]
          req = genClientRequestFromUrlComponents scheme comps
      C.port req `shouldBe` 443
    it "generates a request on the supplied port for an https uri with a provided port" $ do
      let (scheme, comps) = createRequest [https|https://google.com:8443|]
          req = genClientRequestFromUrlComponents scheme comps
      C.port req `shouldBe` 8443
    it "generates a request with the correct path (simple)" $ do
      let (scheme, comps) = createRequest [https|https://google.com/my/path/is/great|]
          req = genClientRequestFromUrlComponents scheme comps
      C.path req `shouldBe` "/my/path/is/great"
    it "generates a request with the correct path (unicode)" $ do
      let (scheme, comps) = createRequest [https|https://google.com/my/path/is/great%F0%9F%98%88|]
          req = genClientRequestFromUrlComponents scheme comps
      C.path req `shouldBe` "/my/path/is/great%F0%9F%98%88"
    it "generates a request with the correct query params (simple)" $ do
      let (scheme, comps) = createRequest [https|https://google.com?test=abc|]
          req = genClientRequestFromUrlComponents scheme comps
      C.queryString req `shouldBe` "?test=abc"
    it "generates a request with the correct query params (unicode)" $ do
      let (scheme, comps) = createRequest [https|https://google.com?test=%F0%9F%98%80|]
          req = genClientRequestFromUrlComponents scheme comps
      C.queryString req `shouldBe` "?test=%F0%9F%98%80"
