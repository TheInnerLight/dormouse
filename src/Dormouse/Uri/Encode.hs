{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dormouse.Uri.Encode
  ( encodeQuery
  , encodePath
  , encodeUnless
  ) where

import Data.Char (chr)
import qualified Data.List as L
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Dormouse.Uri.Types
import Dormouse.Uri.RFC3986

-- | Percent encode a word8 as an ascii 'Bytestring'
percentEncode :: Word8 -> B.ByteString
percentEncode w = 
  let h = w `div` 16
      l = w `mod` 16 in
  B.pack [37, hex h, hex l]
  where 
    hex x
     | x < 10    = 48+x
     | otherwise = 55+x

-- | Percent encode all chars in the supplied text except for those which satsify the supplied predicate
encodeUnless :: (Char -> Bool)  -> T.Text -> B.ByteString
encodeUnless isAllowedChar = B.concatMap pEncodeQuery . E.encodeUtf8
  where
    pEncodeQuery :: Word8 -> B.ByteString
    pEncodeQuery c
      | isAllowedChar (chr $ fromEnum c) = B.singleton c
      | otherwise                         = percentEncode c

-- | Generate an ascii `Bytestring` from a supplied Query by percent encoding all of the invalid octets
encodeQuery :: Query -> B.ByteString
encodeQuery = B.append "?" . encodeUnless isQueryChar . unQuery

-- | Generate an ascii `Bytestring` from a supplied Path by percent encoding all of the invalid octets
encodePath :: Path 'Absolute -> B.ByteString
encodePath =  B.append "/" . B.intercalate "/" . fmap (encodeUnless isPathChar . unPathSegment) . unPath
