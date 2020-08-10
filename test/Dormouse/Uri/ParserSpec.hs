{-# LANGUAGE DataKinds #-}
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
uriWithHostAndPath = AbsOrRelUri (AbsoluteUri (Scheme "http") (Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "google.com", authorityPort = Nothing}) (Path [PathSegment "test1", PathSegment "test2"]) Nothing Nothing )

uriWithHostUsernameAndPath :: Uri 'Unknown a
uriWithHostUsernameAndPath = AbsOrRelUri (AbsoluteUri (Scheme "http") (Just $ Authority {authorityUserInfo = Just (UserInfo {userInfoUsername = Username "j.t.kirk", userInfoPassword = Nothing}), authorityHost = Host "google.com", authorityPort = Nothing}) (Path [PathSegment "test1", PathSegment "test2"]) Nothing Nothing )

uriWithHostUsernamePasswordAndPath :: Uri 'Unknown a
uriWithHostUsernamePasswordAndPath = AbsOrRelUri (AbsoluteUri (Scheme "http") (Just $ Authority {authorityUserInfo = Just (UserInfo {userInfoUsername = Username "j.t.kirk", userInfoPassword = Just $ Password "11a"}), authorityHost = Host "google.com", authorityPort = Nothing}) (Path [PathSegment "test1", PathSegment "test2"]) Nothing Nothing )

uriWithHostPathAndPort :: Uri 'Unknown a
uriWithHostPathAndPort = AbsOrRelUri (AbsoluteUri (Scheme "https") (Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "www.example.com", authorityPort = Just 123}) (Path [PathSegment "forum", PathSegment "questions", PathSegment ""]) Nothing Nothing )

uriWithHostPathQueryAndFragment :: Uri 'Unknown a
uriWithHostPathQueryAndFragment = AbsOrRelUri (AbsoluteUri (Scheme "https") (Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "www.example.com", authorityPort = Nothing}) (Path [PathSegment "forum", PathSegment "questions", PathSegment ""]) (Just $ Query "tag=networking&order=newest") (Just $ Fragment "top") )

ldapUri :: Uri 'Unknown a
ldapUri = AbsOrRelUri (AbsoluteUri (Scheme "ldap") (Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "192.168.0.1", authorityPort = Nothing}) (Path [PathSegment "c=GB"]) (Just $ Query "objectClass?one") Nothing )

telUri :: Uri 'Unknown a
telUri = AbsOrRelUri (AbsoluteUri (Scheme "tel") Nothing (Path [PathSegment "+1-816-555-1212"]) Nothing Nothing )


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


