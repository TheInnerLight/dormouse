{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Dormouse.Uri.Parser
  ( pUri
  , pAbsoluteUri
  , pRelativeUri
  , pScheme
  , pUsername
  , pPassword
  , pUserInfo
  , pIPv4
  , pRegName
  , pHost
  , pPort
  , pAuthority
  , pPathAbsAuth
  , pPathAbsNoAuth
  , pPathRel
  , pQuery
  , pFragment
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

pIPv4 :: Parser T.Text
pIPv4 = do
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
      i | i > 255 -> fail "IPv4 Octects must be in range 0-255"
      i           -> return i

pHost :: Parser Host
pHost = do
  hostText <- pIPv4 <|> pRegName
  return . Host  $ hostText

pUserInfo :: Parser UserInfo
pUserInfo = do
  username <- pUsername
  password <- pMaybe (char ':' *> pPassword)
  _ <- char '@'
  return $ UserInfo { userInfoUsername = username, userInfoPassword = password }

pPort :: Parser Int
pPort = 
  (char ':' *> decimal) >>= \case
    i | i > 65535 -> fail "Port must be in the range 0-65535"
    i             -> return i

pAuthority :: Parser Authority
pAuthority = do
  _ <- string "//"
  authUserInfo <- pMaybe pUserInfo
  authHost <- pHost
  authPort <- pMaybe pPort
  _ <- peekChar >>= \case
    Nothing                                   -> return ()
    Just c | c == '/' || c == '?' || c == '#' -> return ()
    _                                         -> fail "Invalid authority termination character, must be /, ?, # or end of input"
  return Authority { authorityUserInfo = authUserInfo, authorityHost = authHost, authorityPort = authPort}

pPathChar :: Parser Char 
pPathChar = satisfy isPathChar <|> pPercentEnc

pPathCharNc :: Parser Char 
pPathCharNc = satisfy isPathCharNoColon <|> pPercentEnc

pSegmentNz :: Parser PathSegment 
pSegmentNz = fmap (PathSegment . repack) $ many1' pPathChar

pSegmentNzNc :: Parser PathSegment 
pSegmentNzNc = fmap (PathSegment . repack) $ many1' pPathCharNc

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
pPathRel = fmap (Path) (pPathsAbsolute <|> pPathsNoScheme <|> pPathsEmpty)

pQuery :: Parser (Query)
pQuery = do
  queryText <- (char '?' *> (many1' (satisfy isQueryChar <|> pPercentEnc)))
  _ <- peekChar >>= \case
    Nothing           -> return ()
    Just c | c == '#' -> return ()
    c                 -> fail $ "Invalid query termination character: " <> show c <> ", must be # or end of input"
  return . Query . repack $ queryText

pFragment :: Parser (Fragment)
pFragment = do
  fragmentText <- (char '#' *> (many1' (satisfy isFragmentChar <|> pPercentEnc)))
  _ <- peekChar >>= \case
    Nothing           -> return ()
    c                 -> fail $ "Invalid fragment termination character: " <> show c <> ", must be end of input"
  return . Fragment . repack $ fragmentText

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

pRelativeUri :: Parser Uri
pRelativeUri = do
  path <- pPathRel
  query <- pMaybe pQuery
  fragment <- pMaybe pFragment
  _ <- endOfInput
  return $ RelativeUri $ RelUri { uriPath = path, uriQuery = query, uriFragment = fragment }

pAbsoluteUri :: Parser Uri
pAbsoluteUri = do
  (scheme, authority) <- pAbsolutePart
  path <- if isJust authority then pPathAbsAuth else pPathAbsNoAuth
  query <- pMaybe pQuery
  fragment <- pMaybe pFragment
  _ <- endOfInput
  return $ AbsoluteUri $ AbsUri {uriScheme = scheme, uriAuthority = authority, uriPath = path, uriQuery = query, uriFragment = fragment }

pUri :: Parser Uri
pUri = pAbsoluteUri <|> pRelativeUri
