{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Dormouse.Uri.Parser
  ( pUri
  , pAbsoluteUri
  , pRelativeUri
  ) where

import Control.Applicative ((<|>))
import Data.Char as C
import Data.CaseInsensitive (CI, mk)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Dormouse.Uri.Types

isGenDelim :: Char -> Bool
isGenDelim c = c == ':' || c == '/' || c == '?' || c == '#' || c == '[' || c == ']' || c == '@'

isSubDelim :: Char -> Bool
isSubDelim c = c == '!' || c == '$' || c == '&' || c == '\'' || c == '(' || c == ')' || c == '*' || c == '+' || c == ',' || c == ';' || c == '='

isReserved :: Char -> Bool
isReserved c = isGenDelim c || isSubDelim c

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = C.isAlpha c && C.isAscii c

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric c = C.isAlphaNum c && C.isAscii c

isUnreserved :: Char -> Bool
isUnreserved c = isAsciiAlphaNumeric c || c == '-' || c == '.' || c == '_' || c == '~'

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

pPercentEnc :: Parser Char
pPercentEnc = do
  _ <- (char '%')
  hexdig1 <- hexadecimal
  hexdig2 <- hexadecimal
  return . chr $ hexdig1 * 16 + hexdig2

pUsername :: Parser Username
pUsername = do
  xs <- many1' (pUnreserved <|> pPercentEnc <|> pSubDelim)
  return $ Username (T.pack xs)

pPassword :: Parser Password
pPassword = do
  xs <- many1' (pUnreserved <|> pPercentEnc <|> pSubDelim <> char ':')
  return $ Password (T.pack xs)

pRegName :: Parser T.Text
pRegName = do
  xs <- many1' (pUnreserved <|> pPercentEnc <|> pSubDelim)
  return . T.pack $ xs

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
pPathChar = pUnreserved <|> pPercentEnc <|> pSubDelim <|> char ':' <|> char '@'

pSegmentNz :: Parser PathSegment 
pSegmentNz = fmap (PathSegment . T.pack) $ many1' pPathChar

pSegmentNzNc :: Parser PathSegment 
pSegmentNzNc = fmap (PathSegment . T.pack) $ many1' (pUnreserved <|> pPercentEnc <|> pSubDelim <|> char '@')

pSegment :: Parser PathSegment
pSegment = fmap (PathSegment . T.pack) $ many' pPathChar

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
  let maybeQueryText = pMaybe (char '?' *> (many1' (pPathChar <|> char '/' <|> char '?'))) in
  fmap (fmap (Query . T.pack)) $ maybeQueryText

pFragment :: Parser (Maybe Fragment)
pFragment = 
  let maybeQueryText = pMaybe (char '#' *> (many1' (pPathChar <|> char '/' <|> char '?'))) in
  fmap (fmap (Fragment . T.pack)) $ maybeQueryText

pScheme :: Parser Scheme
pScheme = do
  x <- pAsciiAlpha
  xs <- many' (pAsciiAlphaNumeric <|> char '+' <|> char '.' <|> char '-' )
  _ <- char ':'
  return $ Scheme (T.toLower . T.pack $ x:xs)


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
