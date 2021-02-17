{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dormouse.Generators.UriComponents 
  ( genValidScheme
  , genInvalidScheme
  , genValidUserInfo
  , genInvalidUserInfo
  , genValidIPv4
  , genValidRegName
  , genValidHost
  , genValidPort
  , genValidAuthority
  , genValidPathAbsAuth
  , genValidPathAbsNoAuth
  , genValidPathRel
  , genValidQuery
  , genValidFragment
  , genValidUri
  , genValidUriRef
  )
  where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as C
import qualified Data.Text as T
import Dormouse.Uri.Encode
import Dormouse.Uri.RFC3986
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genPercentEncoded :: Gen B.ByteString
genPercentEncoded = do
  char <- Gen.filter C.isPrint Gen.unicode
  let t = T.pack $ [char]
  let percentEncoded = encodeUnless (const False) t
  return $ percentEncoded

genValidScheme :: Gen B.ByteString
genValidScheme = do
  first <- Gen.filter isAsciiAlpha Gen.ascii
  bs <- Gen.list (Range.constant 0 10) (Gen.filter isSchemeChar Gen.ascii)
  return . B8.pack $ (first : bs ++ [':'])

data SchemeFailureMode 
  = NonAsciiFirstChar
  | InvalidSchemeChar
  | NoTrailingSemicolon

schemeFailureMode :: Int -> SchemeFailureMode
schemeFailureMode 1 = NonAsciiFirstChar
schemeFailureMode 2 = InvalidSchemeChar
schemeFailureMode 3 = NoTrailingSemicolon
schemeFailureMode _ = undefined

genInvalidScheme :: Gen B.ByteString
genInvalidScheme = do
  failureMode <- fmap schemeFailureMode $ Gen.element [1..3]
  first <- case failureMode of
    NonAsciiFirstChar -> Gen.filter (not . isAsciiAlpha) Gen.ascii
    _                 -> Gen.filter isAsciiAlpha Gen.ascii
  remainder <- case failureMode of
    InvalidSchemeChar -> do
      let c = Gen.filter (\x -> (not $ isSchemeChar x) && x /= ':') Gen.ascii
      Gen.list (Range.constant 1 10) c
    _ -> do
      let c = Gen.filter isSchemeChar Gen.ascii
      Gen.list (Range.constant 0 10) c
  let finalBs = case failureMode of
        NoTrailingSemicolon -> first : remainder
        _                   -> first : remainder ++ [':']
  return $ B8.pack finalBs

genUserInfoChar :: Gen B.ByteString
genUserInfoChar = Gen.frequency [(1, genPercentEncoded), (25, fmap (B8.pack . return) $ Gen.filter isUserInfoChar Gen.ascii)]

genValidUserInfo :: Gen B.ByteString
genValidUserInfo = do
  list <- Gen.list (Range.constant 1 20) genUserInfoChar
  return $ B.append (B.intercalate "" list) "@"

genInvalidUserInfo :: Gen B.ByteString
genInvalidUserInfo = do
  invalids <- Gen.list (Range.constant 1 5) genInvalidUserInfoChar
  valids <- Gen.list (Range.constant 0 15) genUserInfoChar
  fmap (B.intercalate "") $ Gen.shuffle $ invalids ++ valids
  where
    genInvalidUserInfoChar = fmap (B8.pack . return) $ Gen.filter (\x -> (not $ isUserInfoChar x) && x /= '%' && C.isPrint x) Gen.ascii

genValidIPv4 :: Gen B.ByteString
genValidIPv4 = do
  ipChars <- (\a b c d -> show a <> "." <> show b <> "." <> show c <> "." <> show d) <$> genOctet <*> genOctet <*> genOctet <*> genOctet
  return $ B8.pack ipChars
  where
    genOctet = Gen.word8 Range.constantBounded

genValidRegName :: Gen B.ByteString
genValidRegName = Gen.frequency [(1, genPercentEncoded), (25, fmap (B8.pack . return) $ Gen.filter isRegNameChar Gen.ascii)]

genValidHost :: Gen B.ByteString
genValidHost = Gen.choice [genValidIPv4, genValidRegName]

genValidPort :: Gen B.ByteString
genValidPort = fmap (B.append ":" . B8.pack . show) $ Gen.word16 Range.constantBounded

genValidAuthority :: Gen B.ByteString
genValidAuthority = do
  maybeUserInfo <- Gen.maybe genValidUserInfo
  let userInfoPrefix = maybe B.empty id maybeUserInfo
  host <- genValidHost
  maybePort <- Gen.maybe genValidPort
  let portSuffix = maybe B.empty id maybePort
  return . B.append "//" . B.append userInfoPrefix $ B.append host portSuffix

genPathChar :: Gen B.ByteString
genPathChar = Gen.frequency [(1, genPercentEncoded), (25, fmap (B8.pack . return) $ Gen.filter isPathChar Gen.ascii)]

genPathSegment :: Gen B.ByteString
genPathSegment = fmap (B.intercalate "") $ Gen.list (Range.constant 0 20) genPathChar

genPathSegmentNz :: Gen B.ByteString
genPathSegmentNz = fmap (B.intercalate "") $ Gen.list (Range.constant 1 20) genPathChar

genPathCharNc :: Gen B.ByteString
genPathCharNc = Gen.frequency [(1, genPercentEncoded), (25, fmap (B8.pack . return) $ Gen.filter isPathCharNoColon Gen.ascii)]

genPathSegmentNzNc :: Gen B.ByteString
genPathSegmentNzNc = fmap (B.intercalate "") $ Gen.list (Range.constant 1 20) genPathCharNc

genPathsAbEmpty :: Gen B.ByteString
genPathsAbEmpty = do
  components <- Gen.list (Range.constant 1 10) genPathSegment
  return . B.append "/" $ B.intercalate "/" components

genPathsAbsolute :: Gen B.ByteString
genPathsAbsolute = do
  first <- genPathSegmentNz
  components <- Gen.list (Range.constant 0 10) genPathSegment
  return . B.append "/" . B.append first $ B.intercalate "/" components

genPathsNoScheme :: Gen B.ByteString
genPathsNoScheme = do
  first <- genPathSegmentNzNc
  components <- Gen.list (Range.constant 0 10) genPathSegment
  return . B.append first . B.append "/" $ B.intercalate "/" components

genPathsRootless :: Gen B.ByteString
genPathsRootless = do
  first <- genPathSegmentNz
  components <- Gen.list (Range.constant 0 10) genPathSegment
  return . B.append first . B.append "/" $ B.intercalate "/" components

genValidPathsEmpty :: Gen B.ByteString
genValidPathsEmpty = return B.empty

genValidPathAbsAuth :: Gen B.ByteString
genValidPathAbsAuth = Gen.choice [genPathsAbEmpty]

genValidPathAbsNoAuth :: Gen B.ByteString
genValidPathAbsNoAuth = Gen.choice [genPathsAbsolute, genPathsRootless, genValidPathsEmpty]

genValidPathRel :: Gen B.ByteString
genValidPathRel = Gen.choice [genPathsAbsolute, genPathsNoScheme, genValidPathsEmpty]

genQueryChar :: Gen B.ByteString
genQueryChar = Gen.frequency [(1, genPercentEncoded), (25, fmap (B8.pack . return) $ Gen.filter isQueryChar Gen.ascii)]

genValidQuery :: Gen B.ByteString
genValidQuery = do
  list <- Gen.list (Range.constant 1 50) genQueryChar
  return $ B.append "?" $ B.intercalate "" list

genFragmentChar :: Gen B.ByteString
genFragmentChar = Gen.frequency [(1, genPercentEncoded), (25, fmap (B8.pack . return) $ Gen.filter isFragmentChar Gen.ascii)]

genValidFragment :: Gen B.ByteString
genValidFragment = do
  list <- Gen.list (Range.constant 1 50) genFragmentChar
  return $ B.append "#" $ B.intercalate "" list

genValidUri :: Gen B.ByteString
genValidUri = do
  scheme <- genValidScheme
  authority <- Gen.maybe genValidAuthority
  path <- case authority of
    Just _  -> genValidPathAbsAuth
    Nothing -> genValidPathAbsNoAuth
  query <- Gen.maybe genValidQuery
  fragment <- Gen.maybe genValidFragment
  return . B.intercalate "" $ [scheme, maybe B.empty id authority, path, maybe B.empty id query, maybe B.empty id fragment]

genValidRelRef :: Gen B.ByteString
genValidRelRef = do
  authority <- Gen.maybe genValidAuthority
  path <- case authority of
    Just _  -> genValidPathAbsAuth
    Nothing -> genValidPathRel
  query <- Gen.maybe genValidQuery
  fragment <- Gen.maybe genValidFragment
  return . B.intercalate "" $ [maybe B.empty id authority, path, maybe B.empty id query, maybe B.empty id fragment]

genValidUriRef :: Gen B8.ByteString
genValidUriRef = Gen.choice [genValidUri, genValidRelRef]
