{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dormouse.Uri.Parser
  ( id
  ) where

import Control.Applicative ((<|>))
import Data.Char as C
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
  xs <- many' (pUnreserved <|> pPercentEnc <|> pSubDelim)
  return $ Username (T.pack xs)

pPassword :: Parser Password
pPassword = do
  xs <- many' (pUnreserved <|> pPercentEnc <|> pSubDelim <> char ':')
  return $ Password (T.pack xs)

pRegName :: Parser T.Text
pRegName = do
  xs <- many' (pUnreserved <|> pPercentEnc <|> pSubDelim)
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
  hostText <- choice [pRegName, pIPV4]
  return . Host  $ hostText

pUserInfo :: Parser UserInfo
pUserInfo = do
  username <- pUsername
  password <- pMaybe pPassword
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

pSegmentNz :: Parser Char 
pSegmentNz = pPathChar

pSegmentNzNc :: Parser Char 
pSegmentNzNc = pUnreserved <|> pPercentEnc <|> pSubDelim <|> char '@'

pSegment :: Parser PathSegment
pSegment = fmap (PathSegment . T.pack) $ many' pPathChar

pPathsAbEmpty :: Parser [PathSegment]
pPathsAbEmpty = pSegment `sepBy` (char '/')

pPathsAbsolute :: Parser [PathSegment]
pPathsAbsolute = do
  slash <- char '/'
  seg <- pSegmentNz
  comps <- pSegment `sepBy` (char ',')
  return comps

pPathsNoScheme :: Parser [PathSegment]
pPathsNoScheme = do
  seg <- pSegmentNzNc
  comps <- pSegment `sepBy` (char ',')
  return comps

pPathsRootless :: Parser [PathSegment]
pPathsRootless = do
  seg <- pSegmentNz
  comps <- pSegment `sepBy` (char ',')
  return comps

pPathsEmpty :: Parser [PathSegment]
pPathsEmpty = return []

pPath :: Parser Path
pPath = fmap (Path) $ pPathsAbEmpty <|> pPathsAbsolute <|> pPathsNoScheme <|> pPathsRootless <|> pPathsEmpty

pScheme :: Parser Scheme
pScheme = do
  x <- pAsciiAlpha
  xs <- many' (pAsciiAlphaNumeric <|> char '+' <|> char '.' <|> char '-' )
  _ <- char ':'
  return $ Scheme (T.pack $ x:xs)

pAbsolutePart :: Parser (Scheme, Authority)
pAbsolutePart = do
  scheme <- pScheme
  authority <- pAuthority
  return (scheme, authority)

pRelativeUri :: Parser (Uri Relative scheme)
pRelativeUri = do
  path <- pPath
  return $ RelativeUri path [] (Fragment "")

pAbsoluteUri :: Parser (Uri Absolute scheme)
pAbsoluteUri = do
  (scheme, authority) <- pAbsolutePart
  path <- pPath
  let relPath = RelativeUri path [] (Fragment "")
  return $ AbsoluteUri scheme authority relPath

pUri :: Parser (Uri Unknown scheme)
pUri = do
  maybeAbsPart <- pMaybe pAbsolutePart
  relativeUri <- pRelativeUri
  return $ maybe (UnknownRel $ relativeUri) (\(scheme, authority) -> UnknownAbs $ AbsoluteUri scheme authority relativeUri) maybeAbsPart

