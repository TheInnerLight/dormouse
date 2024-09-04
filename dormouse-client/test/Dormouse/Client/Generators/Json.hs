{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dormouse.Client.Generators.Json 
  ( genJsonValue
  , JsonGenRanges(..)
  )
  where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Scientific as S
import qualified Data.Vector as V
import qualified Data.Char as C
import Hedgehog
import qualified Hedgehog.Gen as Gen

data JsonGenRanges = JsonGenRanges 
  { stringRanges :: Range Int
  , doubleRanges :: Range Double
  , arrayLenRanges :: Range Int
  }

genJsonNull :: Gen A.Value
genJsonNull = pure A.Null

genJsonString :: Range Int -> Gen A.Value
genJsonString sr = fmap A.String $ Gen.text sr jsonChar

jsonChar :: Gen Char
jsonChar = Gen.filter (\c -> c /= '\"' && c /= '\\' && (not $ C.isControl c) && c /= '\r' && c /= '\n') Gen.unicode

genJsonBool :: Gen A.Value
genJsonBool = fmap A.Bool Gen.bool

genJsonNumber :: Range Double -> Gen A.Value
genJsonNumber r = fmap (A.Number . S.fromFloatDigits) $ Gen.double r

genJsonArray :: JsonGenRanges -> Gen A.Value
genJsonArray ranges = fmap (A.Array . V.fromList) $ Gen.list ar gen
  where
    gen = Gen.recursive Gen.choice [genJsonBool, genJsonNumber nr, genJsonString sr] [genJsonValue ranges, genJsonObject ranges]
    nr = doubleRanges ranges
    sr = stringRanges ranges
    ar = arrayLenRanges ranges

genJsonObject :: JsonGenRanges -> Gen A.Value
genJsonObject ranges = fmap A.object $ Gen.list ar genNameValue
  where
    genNameValue = do
      name <- Gen.text sr jsonChar
      value <- genJsonValue ranges
      return (AK.fromText name, value)
    sr = stringRanges ranges
    ar = arrayLenRanges ranges

genJsonValue :: JsonGenRanges -> Gen A.Value
genJsonValue ranges = Gen.choice [genJsonNull, genJsonString (stringRanges ranges), genJsonBool, genJsonNumber (doubleRanges ranges), genJsonArray ranges, genJsonObject ranges]
