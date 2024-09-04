{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Dormouse.Uri.Parser
  ( pUri
  , pUriRef
  , pRelativeUri
  , pScheme
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
  , percentDecode
  ) where

import Data.Word ( Word8 ) 
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Bits (shiftL, (.|.))
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Dormouse.Uri.Types
import Dormouse.Uri.RFC3986
import qualified Data.ByteString as B

pMaybe :: Parser a -> Parser (Maybe a)
pMaybe p = option Nothing (Just <$> p)

pAsciiAlpha :: Parser Char
pAsciiAlpha = satisfy isAsciiAlpha

data PDState = Percent | Hex1 Word8 | Other | PDError

percentDecode :: B.ByteString -> Maybe B.ByteString
percentDecode xs =
  if B.elem 37 xs then
    case B.foldl' f (B.empty, Other) xs of
      (_, PDError)  -> Nothing 
      (bs, _)       -> Just bs
  else 
    Just xs
  where
    f (es, Percent) e                                     = (es, Hex1 e)
    f (es, Hex1 e1) e2 | isHexDigit' e1 && isHexDigit' e2 = (B.snoc es (hexToWord8 e1 `shiftL` 4 .|. hexToWord8 e2), Other)
    f (es, Hex1 _)  _                                     = (es, PDError)
    f (es, Other)   37                                    = (es, Percent)
    f (es, Other)   e                                     = (B.snoc es e, Other)
    f (es, PDError) _                                     = (es, PDError)
    hexToWord8 w | w >= 48 && w <= 57 = fromIntegral (w - 48)
                 | w >= 97            = fromIntegral (w - 87)
                 | otherwise          = fromIntegral (w - 55)
    isHexDigit' w = (w >= 48 && w <= 57) ||  (w >= 97 && w <= 102) ||(w >= 65 && w <= 70)

takeWhileW8 :: (Char -> Bool) -> Parser B.ByteString 
takeWhileW8 f = AB.takeWhile (f . BS.w2c)

takeWhile1W8 :: (Char -> Bool) -> Parser B.ByteString 
takeWhile1W8 f = AB.takeWhile1 (f . BS.w2c)

pUserInfo :: Parser UserInfo
pUserInfo = do
  xs <- takeWhileW8 (\x -> isUserInfoChar x || x == '%')
  xs' <- maybe (fail "Failed to percent-decode") pure $ percentDecode xs
  _ <- char '@'
  return $ UserInfo (TE.decodeUtf8Lenient xs')

pRegName :: Parser T.Text
pRegName = do
  xs <- takeWhileW8 (\x -> isRegNameChar x || x == '%')
  xs' <- maybe (fail "Failed to percent-decode") pure $ percentDecode xs
  return . TE.decodeUtf8Lenient $ xs'

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
  hostText <- pRegName <|> pIPv4
  return . Host  $ hostText

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

pPathAbsAuth :: Parser (Path rt)
pPathAbsAuth = do
  p <- takeWhileW8 (\x -> isPathChar x || x == '%' || x == '/')
  p' <- maybe (fail "Failed to percent-decode") pure $ percentDecode p
  let ps = PathSegment <$> T.split (== '/') (TE.decodeUtf8Lenient p')
  case ps of -- begins with "/" is empty
    (PathSegment x):xs | T.null x -> return $ Path xs
    (PathSegment _):_             -> fail "must begin with /"
    xs                            -> return $ Path xs

pPathAbsNoAuth :: Parser (Path 'Absolute)
pPathAbsNoAuth = do
  p <- takeWhileW8 (\x -> isPathChar x || x == '%' || x == '/')
  p' <- maybe (fail "Failed to percent-decode") pure $ percentDecode p
  let ps = PathSegment <$> T.split (== '/') (TE.decodeUtf8Lenient p')
  case ps of -- begins with "/" but not "//" OR begins with segment OR empty
    (PathSegment x1):(PathSegment x2):_ | T.null x1 && T.null x2 -> fail "cannot begin with //"
    (PathSegment x):xs                  | T.null x               -> return $ Path xs
    xs                                                           -> return $ Path xs

pPathRel :: Parser (Path 'Relative)
pPathRel = do
  p <- takeWhileW8 (\x -> isPathChar x || x == '%' || x == '/')
  p' <- maybe (fail "Failed to percent-decode") pure $ percentDecode p
  let ps = PathSegment <$> T.split (== '/') (TE.decodeUtf8Lenient p')
  case ps of
    (PathSegment x1):(PathSegment x2):_ | T.null x1 && T.null x2 -> fail "cannot begin with //"
    (PathSegment x):_                   | T.isPrefixOf ":" x     -> fail "first character of a relative path cannot be :"
    (PathSegment x):xs                  | T.null x               -> return $ Path xs
    xs                                                           -> return $ Path xs

pQuery :: Parser Query
pQuery = do
  qt <- char '?' *> takeWhile1W8 (\x -> isQueryChar x || x == '%')
  queryText <- maybe (fail "Failed to percent-decode") pure $ percentDecode qt
  _ <- peekChar >>= \case
    Nothing           -> return ()
    Just c | c == '#' -> return ()
    c                 -> fail $ "Invalid query termination character: " <> show c <> ", must be # or end of input"
  return . Query . TE.decodeUtf8Lenient $ queryText

pFragment :: Parser Fragment
pFragment = do
  ft <- char '#' *> takeWhile1W8 (\x -> isFragmentChar x || x == '%')
  fragmentText <- maybe (fail "Failed to percent-decode") pure $ percentDecode ft
  _ <- peekChar >>= \case
    Nothing           -> return ()
    c                 -> fail $ "Invalid fragment termination character: " <> show c <> ", must be end of input"
  return . Fragment . TE.decodeUtf8Lenient $ fragmentText

pScheme :: Parser Scheme
pScheme = do
  x <- pAsciiAlpha
  xs <- A.takeWhile isSchemeChar
  _ <- char ':'
  return $ Scheme (T.toLower . TE.decodeUtf8Lenient $ B.cons (BS.c2w x) xs)

pAbsolutePart :: Parser (Scheme, Maybe Authority)
pAbsolutePart = do
  scheme <- pScheme
  authority <- pMaybe pAuthority
  return (scheme, authority)

pRelativeUri :: Parser RelRef
pRelativeUri = do
  authority <- pMaybe pAuthority
  path <- if isJust authority then pPathAbsAuth else pPathRel
  query <- pMaybe pQuery
  fragment <- pMaybe pFragment
  _ <- endOfInput
  return  $ RelRef { relRefAuthority = authority, relRefPath = path, relRefQuery = query, relRefFragment = fragment }

pUri :: Parser Uri
pUri = do
  (scheme, authority) <- pAbsolutePart
  path <- if isJust authority then pPathAbsAuth else pPathAbsNoAuth
  query <- pMaybe pQuery
  fragment <- pMaybe pFragment
  _ <- endOfInput
  return $ Uri {uriScheme = scheme, uriAuthority = authority, uriPath = path, uriQuery = query, uriFragment = fragment }

pUriRef :: Parser UriReference
pUriRef = (AbsoluteUri <$> pUri) <|> (RelativeRef <$> pRelativeUri)
