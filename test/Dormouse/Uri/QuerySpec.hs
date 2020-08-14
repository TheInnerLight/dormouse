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
import Dormouse.Uri.QQ


uriWithDestructSequence :: Uri 'Absolute a
uriWithDestructSequence = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "enterprise.starfleet.com", authorityPort = Nothing}
  , uriPath = Path ["destruct"]
  , uriQuery = Just $ "code1=11a&code2=11a2b&code3=1b2b3&code4=000destruct-0"
  , uriFragment = Nothing
  }

uriLookingUpTheEnterprise :: Uri 'Absolute a
uriLookingUpTheEnterprise = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "starfleet.com", authorityPort = Nothing}
  , uriPath = Path ["ship"]
  , uriQuery = Just $ "registry=1701"
  , uriFragment = Nothing
  }

uriUnicodeInPathText :: Uri 'Absolute a
uriUnicodeInPathText = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "starfleet.com", authorityPort = Nothing}
  , uriPath = Path ["ship"]
  , uriQuery = Just $ "happy=😄"
  , uriFragment = Nothing
  }

uriSpacesInPathText :: Uri 'Absolute a
uriSpacesInPathText = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "starfleet.com", authorityPort = Nothing}
  , uriPath = Path ["test"]
  , uriQuery = Just $ "test=      dsfsdfdsf      "
  , uriFragment = Nothing
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
    it "generates correct uri using the URI builder syntax with unicode in path text" $ do
      let actualUri = [http|http://starfleet.com|] </> "ship" ? ("happy" =: ("😄" :: T.Text))
      actualUri `shouldBe` uriUnicodeInPathText
    it "generates correct uri using the URI builder syntax with spaces in path text" $ do
      let actualUri = [http|http://starfleet.com|] </> "test" ? ("test" =: ("      dsfsdfdsf      " :: T.Text))
      actualUri `shouldBe` uriSpacesInPathText

