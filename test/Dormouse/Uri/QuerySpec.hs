{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Dormouse.Uri.QuerySpec
  ( tests
  ) where

import qualified Data.Text as T
import Test.Hspec
import Dormouse.Uri
import Dormouse.Url
import Dormouse.Url.Builder
import Dormouse.Url.QQ

uriWithDestructSequence :: Url "http"
uriWithDestructSequence = HttpUrl $ UrlComponents
  { urlAuthority = Authority {authorityUserInfo = Nothing, authorityHost = Host "enterprise.starfleet.com", authorityPort = Nothing}
  , urlPath = Path ["destruct"]
  , urlQuery = Just $ "code1=11a&code2=11a2b&code3=1b2b3&code4=000destruct-0"
  , urlFragment = Nothing
  }

uriLookingUpTheEnterprise :: Url "http"
uriLookingUpTheEnterprise = HttpUrl $ UrlComponents
  { urlAuthority = Authority {authorityUserInfo = Nothing, authorityHost = Host "starfleet.com", authorityPort = Nothing}
  , urlPath = Path ["ship"]
  , urlQuery = Just $ "registry=1701"
  , urlFragment = Nothing
  }

tests :: IO()
tests = hspec $
  describe "build query" $ do
    it "generates correct uri using the URI builder syntax with text components" $ do
      let actualUri = [http|http://enterprise.starfleet.com|] </> "destruct" ? ("code1" =: ("11a" :: T.Text)) & ("code2" =: ("11a2b" :: T.Text)) & ("code3" =: ("1b2b3":: T.Text)) & ("code4" =: ("000destruct-0" :: T.Text))
      actualUri `shouldBe` uriWithDestructSequence
    it "generates correct uri using the URI builder syntax with int components" $ do
      let actualUri = [http|http://starfleet.com|] </> "ship" ? ("registry" =: (1701 :: Int))
      actualUri `shouldBe` uriLookingUpTheEnterprise


