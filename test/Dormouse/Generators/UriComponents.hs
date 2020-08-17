{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dormouse.Generators.UriComponents 
  ( genValidScheme
  , genInvalidScheme
  )
  where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Dormouse.Uri.RFC3986
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype SchemeByteString = SchemeByteString { unSchemeByteString :: B.ByteString }
  deriving (Eq, Show)

genValidScheme :: Gen B.ByteString
genValidScheme = do
  first <- Gen.filter isAsciiAlpha Gen.ascii
  bs <- Gen.list (Range.constant 0 20) (Gen.filter isSchemeChar Gen.ascii)
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

