{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Dormouse.Uri.ParserSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.Hedgehog
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Char as C
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dormouse.Generators.UriComponents 
import Dormouse.Uri.Types
import Dormouse.Uri.Parser
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


uriWithHostAndPath :: Uri
uriWithHostAndPath = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "http"
  , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityHost = Host "google.com", authorityPort = Nothing}
  , uriPath = Path [PathSegment "test1", PathSegment "test2"]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

uriWithHostUsernameAndPath :: Uri
uriWithHostUsernameAndPath = AbsoluteUri $ AbsUri
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

uriWithHostUsernamePasswordAndPath :: Uri
uriWithHostUsernamePasswordAndPath = AbsoluteUri $ AbsUri
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

uriWithHostPathAndPort :: Uri
uriWithHostPathAndPort = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Just 123 }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

uriWithHostPathQueryAndFragment :: Uri
uriWithHostPathQueryAndFragment = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Nothing }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Just $ Query "tag=networking&order=newest"
  , uriFragment = Just $ Fragment "top"
  }

uriWithUnicodeInQuery :: Uri
uriWithUnicodeInQuery = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Nothing }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Just $ Query "tag=networking&order=newest😀"
  , uriFragment = Nothing
  }

uriWithSpacesInQuery :: Uri
uriWithSpacesInQuery = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Nothing }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Just $ Query "tag=with space"
  , uriFragment = Nothing
  }


uriWithUnicodeInFragment :: Uri
uriWithUnicodeInFragment = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Nothing }
  , uriPath = Path ["forum", "questions", ""]
  , uriQuery = Nothing
  , uriFragment = Just $ "😀😀😀"
  }

uriWithUnicodeInPath :: Uri
uriWithUnicodeInPath = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "https"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "www.example.com", authorityPort = Nothing }
  , uriPath = Path ["test", "dsdsfdsfds😀😀😀", ""]
  , uriQuery = Nothing
  , uriFragment = Nothing 
  }

ldapUri :: Uri
ldapUri = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "ldap"
  , uriAuthority = Just $ Authority { authorityUserInfo = Nothing, authorityHost = "192.168.0.1", authorityPort = Nothing }
  , uriPath = Path [PathSegment "c=GB"]
  , uriQuery = Just $ Query "objectClass?one"
  , uriFragment = Nothing
  }

telUri :: Uri
telUri = AbsoluteUri $ AbsUri
  { uriScheme = Scheme "tel"
  , uriAuthority = Nothing
  , uriPath = Path ["+1-816-555-1212"]
  , uriQuery = Nothing
  , uriFragment = Nothing
  }

infixr 5 :<

pattern b :< bs <- (B.uncons -> Just (b, bs))
pattern Empty   <- (B.uncons -> Nothing)

percentDecode :: B.ByteString -> B.ByteString
percentDecode Empty = B.empty
percentDecode (x :< Empty) = B.singleton x
percentDecode (x :< y :< Empty) = B.cons x $ B.singleton y
percentDecode (p :< x :< y :< xs) 
  | p == c2w '%' = B.cons (fromIntegral $ C.digitToInt (w2c x) * 16 + C.digitToInt (w2c y)) (percentDecode xs)
  | otherwise    = B.cons p $ percentDecode (B.cons x $ B.cons y $ xs)

spec :: Spec
spec = do
  describe "pScheme" $ do
    it "returns the matching scheme for all valid schemes" $ hedgehog $ do
      schemeText <- forAll genValidScheme
      let res = parseOnly (pScheme <* endOfInput) schemeText
      res === (Right . Scheme . T.init . T.toLower $ TE.decodeUtf8 schemeText)
    it "fails for invalid schemes" $ hedgehog $ do
      schemeText <- forAll genInvalidScheme
      let res = parseOnly (pScheme <* endOfInput) schemeText
      isLeft res === True
  describe "pUsername" $ do
    it "returns the matching username for all valid usernames" $ hedgehog $ do
      usernameText <- forAll genValidUsername
      let res = parseOnly (pUsername <* endOfInput) usernameText
      res === (Right . Username . TE.decodeUtf8 . percentDecode $ usernameText)
    it "fails for invalid usernames" $ hedgehog $ do
      usernameText <- forAll genInvalidUsername
      let res = parseOnly (pUsername <* endOfInput) usernameText
      isLeft res === True
  describe "pPassword" $ do
    it "returns the matching password for all valid passwords" $ hedgehog $ do
      passwordText <- forAll genValidPassword
      let res = parseOnly (pPassword <* endOfInput) passwordText
      res === (Right . Password . TE.decodeUtf8 . percentDecode $ passwordText)
    it "fails for invalid passwords" $ hedgehog $ do
      passwordText <- forAll genInvalidPassword
      let res = parseOnly (pPassword <* endOfInput) passwordText
      isLeft res === True
  describe "pUserInfo" $ do
    it "generates a user info for all valid user infos" $ hedgehog $ do
      userInfoText <- forAll genValidUserInfo
      let res = parseOnly (pUserInfo <* endOfInput) userInfoText
      isRight res === True
    it "fails for invalid user infos" $ hedgehog $ do
      userInfoText <- forAll genInvalidUserInfo
      let res = parseOnly (pUserInfo <* endOfInput) userInfoText
      isLeft res === True
  describe "pIPv4" $ do
    it "returns the matching ip address for all valid ip addresses" $ hedgehog $ do
      ipv4Text <- forAll genValidIPv4
      let res = parseOnly (pIPv4 <* endOfInput) ipv4Text
      res === (Right . TE.decodeUtf8 . percentDecode $ ipv4Text)
  describe "pRegName" $ do
    it "returns the matching reg name for all valid reg names" $ hedgehog $ do
      regNameText <- forAll genValidRegName
      let res = parseOnly (pRegName <* endOfInput) regNameText
      res === (Right . TE.decodeUtf8 . percentDecode $ regNameText)
  describe "pHost" $ do
    it "returns the matching host for all valid hosts" $ hedgehog $ do
      hostText <- forAll genValidHost
      let res = parseOnly (pHost <* endOfInput) hostText
      res === (Right . Host . TE.decodeUtf8 . percentDecode $ hostText)
  describe "pPort" $ do
    it "returns the matching host for all valid ports" $ hedgehog $ do
      portText <- forAll genValidPort
      let res = parseOnly (pPort <* endOfInput) portText
      res === (Right . read . T.unpack . T.tail . TE.decodeUtf8 $ portText)
  describe "pAuthority" $ do
    it "generates an authority for all valid authorities" $ hedgehog $ do
      authorityText <- forAll genValidAuthority
      let res = parseOnly (pAuthority <* endOfInput) authorityText
      isRight res === True
  describe "pPathAbsAuth" $ do
    it "generates a path for all valid absolute authority paths" $ hedgehog $ do
      pathText <- forAll genValidPathAbsAuth
      let res = parseOnly (pPathAbsAuth <* endOfInput) pathText
      isRight res === True
  describe "pPathAbsNoAuth" $ do
    it "generates a path for all valid absolute no authority paths" $ hedgehog $ do
      pathText <- forAll genValidPathAbsNoAuth
      let res = parseOnly (pPathAbsNoAuth <* endOfInput) pathText
      isRight res === True
  describe "pPathRel" $ do
    it "generates a path for all valid relative paths" $ hedgehog $ do
      pathText <- forAll genValidPathRel
      let res = parseOnly (pPathRel <* endOfInput) pathText
      isRight res === True
  describe "pQuery" $ do
    it "returns the matching query for all valid queries" $ hedgehog $ do
      queryText <- forAll genValidQuery
      let res = parseOnly (pQuery <* endOfInput) queryText
      res === (Right . Query . T.tail . TE.decodeUtf8 . percentDecode $ queryText)
  describe "pFragment" $ do
    it "returns the matching fragment for all valid fragments" $ hedgehog $ do
      fragmentText <- forAll genValidFragment
      let res = parseOnly (pFragment <* endOfInput) fragmentText
      res === (Right . Fragment . T.tail . TE.decodeUtf8 . percentDecode $ fragmentText)
  describe "pAbsoluteUri" $ do
    it "generates an absolute uri for all valid absolute uris" $ hedgehog $ do
      uriText <- forAll genValidAbsoluteUri
      let res = parseOnly (pAbsoluteUri <* endOfInput) uriText
      isRight res === True
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
    it "generates uri components correctly when there is percent encoded unicode in the query" $ do
      let res = parseOnly pUri "https://www.example.com/forum/questions/?tag=networking&order=newest%F0%9F%98%80"
      res `shouldBe` (Right uriWithUnicodeInQuery)
    it "generates uri components correctly when there are spaces in the query" $ do
      let res = parseOnly pUri "https://www.example.com/forum/questions/?tag=with%20space"
      res `shouldBe` (Right uriWithSpacesInQuery)
    it "generates uri components correctly when there is percent encoded unicode in the fragment" $ do
      let res = parseOnly pUri "https://www.example.com/forum/questions/#%F0%9F%98%80%F0%9F%98%80%F0%9F%98%80"
      res `shouldBe` (Right uriWithUnicodeInFragment)
    it "generates uri components correctly when there is percent encoded unicode in the path" $ do
      let res = parseOnly pUri "https://www.example.com/test/dsdsfdsfds%F0%9F%98%80%F0%9F%98%80%F0%9F%98%80/"
      res `shouldBe` Right uriWithUnicodeInPath
    it "generates uri components correctly for ldap uri" $ do
      let res = parseOnly pUri "ldap://192.168.0.1/c=GB?objectClass?one"
      res `shouldBe` (Right ldapUri)
    it "generates uri components correctly for tel uri" $ do
      let res = parseOnly pUri "tel:+1-816-555-1212"
      res `shouldBe` (Right telUri)
    it "fails for missing scheme" $ do
      let res = parseOnly pUri "://"
      isLeft res `shouldBe` True

