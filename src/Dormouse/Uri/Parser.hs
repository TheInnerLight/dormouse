{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Dormouse.Uri.Parser
  ( pUri
  , pAbsoluteUri
  , pRelativeUri
  , pScheme
  ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 as A
import Data.Char as C
import Data.Bits (Bits, shiftL, (.|.))
import Data.CaseInsensitive (CI, mk)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dormouse.Uri.Types
import Dormouse.Uri.RFC3986
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

repack :: String -> T.Text
repack = TE.decodeUtf8 . B8.pack

pMaybe :: Parser a -> Parser (Maybe a)
pMaybe p = option Nothing (Just <$> p)

pAsciiAlpha :: Parser Char
pAsciiAlpha = satisfy isAsciiAlpha

pAsciiAlphaNumeric :: Parser Char
pAsciiAlphaNumeric = satisfy isAsciiAlphaNumeric

pSubDelim :: Parser Char
pSubDelim = satisfy isSubDelim

pUnreserved :: Parser Char
pUnreserved = satisfy isUnreserved

pSizedHexadecimal :: (Integral a, Bits a) => Int -> Parser a
pSizedHexadecimal n = do
    bytes <- A.take n
    if B.all isHexDigit bytes then return $ B.foldl' step 0 $ bytes else fail "pSizedHexadecimal"
  where 
    isHexDigit w = (w >= 48 && w <= 57) ||  (w >= 97 && w <= 102) ||(w >= 65 && w <= 70)
    step a w | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)

pPercentEnc :: Parser Char
pPercentEnc = do
  _ <- (char '%')
  hexdig1 <- pSizedHexadecimal 1
  hexdig2 <- pSizedHexadecimal 1
  return . chr $ hexdig1 * 16 + hexdig2

pUsername :: Parser Username
pUsername = do
  xs <- many1' (satisfy isUsernameChar <|> pPercentEnc)
  return $ Username (repack xs)

pPassword :: Parser Password
pPassword = do
  xs <- many1' (satisfy isPasswordChar <|> pPercentEnc)
  return $ Password (repack xs)

pRegName :: Parser T.Text
pRegName = do
  xs <- many1' (satisfy isRegNameChar <|> pPercentEnc)
  return . repack $ xs

pIPV4 :: Parser T.Text
pIPV4 = do
  oct1 <- pOctet
  _ <- char '.'
  oct2 <- pOctet
  _ <- char '.'
  oct3 <- pOctet
  _ <- char '.'
  oct4 <- pOctet
  return . T.pack $ show oct1 <> "." <> show oct2 <> "." <> show oct3 <> "." <> show oct4
  where
    pOctet :: Parser Int
    pOctet = decimal >>= \case
      i | i > 256 -> fail "IPv4 Octects must be in range 0-256"
      i           -> return i

pHost :: Parser Host
pHost = do
  hostText <- pIPV4 <|> pRegName
  return . Host  $ hostText

pUserInfo :: Parser UserInfo
pUserInfo = do
  username <- pUsername
  password <- pMaybe (char ':' *> pPassword)
  _ <- char '@'
  return $ UserInfo { userInfoUsername = username, userInfoPassword = password }

pAuthority :: Parser Authority
pAuthority = do
  _ <- string "//"
  authUserInfo <- pMaybe pUserInfo
  authHost <- pHost
  authPort <- pMaybe (char ':' *> decimal)
  return Authority { authorityUserInfo = authUserInfo, authorityHost = authHost, authorityPort = authPort}

pPathChar :: Parser Char 
pPathChar = satisfy isPathChar <|> pPercentEnc

pSegmentNz :: Parser PathSegment 
pSegmentNz = fmap (PathSegment . repack) $ many1' pPathChar

pSegmentNzNc :: Parser PathSegment 
pSegmentNzNc = fmap (PathSegment . repack) $ many1' (pUnreserved <|> pPercentEnc <|> pSubDelim <|> char '@')

pSegment :: Parser PathSegment
pSegment = fmap (PathSegment . repack) $ many' pPathChar

pPathsAbEmpty :: Parser [PathSegment]
pPathsAbEmpty = many1' (char '/' *> pSegment)

pPathsAbsolute :: Parser [PathSegment]
pPathsAbsolute = do
  slash <- char '/'
  seg <- pSegmentNz
  comps <- many' (char '/' *> pSegment)
  return $ seg : comps

pPathsNoScheme :: Parser [PathSegment]
pPathsNoScheme = do
  seg <- pSegmentNzNc
  comps <- many' (char '/' *> pSegment)
  return $ seg : comps

pPathsRootless :: Parser [PathSegment]
pPathsRootless = do
  seg <- pSegmentNz
  comps <- many' (char '/' *> pSegment)
  return $ seg : comps

pPathsEmpty :: Parser [PathSegment]
pPathsEmpty = return []

pPathAbsAuth :: Parser (Path Absolute)
pPathAbsAuth = fmap (Path) (pPathsAbEmpty <|> pPathsAbsolute <|> pPathsEmpty)

pPathAbsNoAuth :: Parser (Path Absolute)
pPathAbsNoAuth = fmap (Path) (pPathsAbsolute <|> pPathsRootless <|> pPathsEmpty)

pPathRel :: Parser (Path Relative)
pPathRel = fmap (Path) (pPathsAbsolute <|> pPathsNoScheme <|> pPathsRootless <|> pPathsEmpty)

pQuery :: Parser (Maybe Query)
pQuery = 
  let maybeQueryText = pMaybe (char '?' *> (many1' (satisfy isQueryChar <|> pPercentEnc))) in
  fmap (fmap (Query . repack)) $ maybeQueryText

pFragment :: Parser (Maybe Fragment)
pFragment = 
  let maybeQueryText = pMaybe (char '#' *> (many1' (satisfy isFragmentChar <|> pPercentEnc))) in
  fmap (fmap (Fragment . repack)) $ maybeQueryText

pScheme :: Parser Scheme
pScheme = do
  x <- pAsciiAlpha
  xs <- many' (pAsciiAlphaNumeric <|> char '+' <|> char '.' <|> char '-' )
  _ <- char ':'
  return $ Scheme (T.toLower . repack $ x:xs)

pAbsolutePart :: Parser (Scheme, Maybe Authority)
pAbsolutePart = do
  scheme <- pScheme
  authority <- pMaybe pAuthority
  return (scheme, authority)

pRelativeUri :: Parser (Uri Relative scheme)
pRelativeUri = do
  path <- pPathRel
  query <- pQuery
  fragment <- pFragment
  _ <- endOfInput
  return $ RelativeUri $ RelUri {uriPath = path, uriQuery = query, uriFragment = fragment}

pAbsoluteUri :: Parser (Uri Absolute scheme)
pAbsoluteUri = do
  (scheme, authority) <- pAbsolutePart
  path <- if isJust authority then pPathAbsAuth else pPathAbsNoAuth
  query <- pQuery
  fragment <- pFragment
  _ <- endOfInput
  return $ AbsoluteUri $ AbsUri {uriScheme = scheme, uriAuthority = authority, uriPath = path, uriQuery = query, uriFragment = fragment }

pUri :: Parser (Uri Unknown scheme)
pUri = fmap AbsOrRelUri pAbsoluteUri <|> fmap AbsOrRelUri pRelativeUri
