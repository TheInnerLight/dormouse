{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Dormouse.Uri.ParserSpec
  ( tests
  ) where

import Test.Hspec
import Data.Attoparsec.Text
import Dormouse.Uri.Types
import Dormouse.Uri.Parser

uriWithHostAndPath :: Uri 'Unknown a
uriWithHostAndPath = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "google.com", authorityPort = Nothing}
  , uriPath = Path [PathSegment "test1", PathSegment "test2"]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

uriWithHostUsernameAndPath :: Uri 'Unknown a
uriWithHostUsernameAndPath = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority 
    { authorityUserInfo = Just (UserInfo {userInfoUsername = "j.t.kirk", userInfoPassword = Nothing})
    , authorityHost = Host "google.com"
    , authorityPort = Nothing
    }
  , uriPath = Path [PathSegment "test1", PathSegment "test2"]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

uriWithHostUsernamePasswordAndPath :: Uri 'Unknown a
uriWithHostUsernamePasswordAndPath = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority 
    { authorityUserInfo = Just (UserInfo {userInfoUsername = "j.t.kirk", userInfoPassword = Just "11a"})
    , authorityHost = Host "google.com"
    , authorityPort = Nothing
    }
  , uriPath = Path [PathSegment "test1", PathSegment "test2"]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

uriWithHostPathAndPort :: Uri 'Unknown a
uriWithHostPathAndPort = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Just 123 }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

uriWithHostPathQueryAndFragment :: Uri 'Unknown a
uriWithHostPathQueryAndFragment = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Nothing }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Just $ Query "tag=networking&order=newest"
  , uriFragment = Just $ Fragment "top"
  }

ldapUri :: Uri 'Unknown a
ldapUri = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "ldap"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "192.168.0.1", authorityPort = Nothing }
  , uriPath = Path [PathSegment "c=GB"]
  , uriQuery = Just $ Query "objectClass?one"
  , uriFragment = Nothing
  }

telUri :: Uri 'Unknown a
telUri = AbsOrRelUri $ AbsoluteUri $ AbsUri
  { uriScheme = Scheme "tel"
  , uriAuthority = Nothing
  , uriPath = Path ["+1-816-555-1212"]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

tests :: IO()
tests = hspec $
  describe "parseURI" $ do
    it "generates uri components correctly for uri with scheme, host and path" $ do
      let res = parseOnly pUri "http://google.com/test1/test2"
      res `shouldBe` (Right uriWithHostAndPath)
    it "generates uri components correctly for uri with upper case scheme, host and path" $ do
      let res = parseOnly pUri "HTTP://google.com/test1/test2"
      res `shouldBe` (Right uriWithHostAndPath)
    it "generates uri components correctly for uri with host, username and path" $ do
      let res = parseOnly pUri "http://j.t.kirk@google.com/test1/test2"
      res `shouldBe` (Right uriWithHostUsernameAndPath)
    it "generates uri components correctly for uri with host, username and path" $ do
      let res = parseOnly pUri "http://j.t.kirk:11a@google.com/test1/test2"
      res `shouldBe` (Right uriWithHostUsernamePasswordAndPath)
    it "generates uri components correctly for uri with host, username, path and port" $ do
      let res = parseOnly pUri "https://www.example.com:123/forum/questions/"
      res `shouldBe` (Right uriWithHostPathAndPort)
    it "generates uri components correctly for uri with host, username, path, port, query and fragment" $ do
      let res = parseOnly pUri "https://www.example.com/forum/questions/?tag=networking&order=newest#top"
      res `shouldBe` (Right uriWithHostPathQueryAndFragment)
    it "generates uri components correctly for ldap uri" $ do
      let res = parseOnly pUri "ldap://192.168.0.1/c=GB?objectClass?one"
      res `shouldBe` (Right ldapUri)
    it "generates uri components correctly for tel uri" $ do
      let res = parseOnly pUri "tel:+1-816-555-1212"
      res `shouldBe` (Right telUri)
