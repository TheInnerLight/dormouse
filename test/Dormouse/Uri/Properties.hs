{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dormouse.Uri.Properties 
  ( SchemeByteString(..)
  , NonSchemeByteString(..)
  )
  where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as C
import Dormouse.Uri.RFC3986
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

newtype SchemeByteString = SchemeByteString { unSchemeByteString :: B.ByteString }
  deriving (Eq, Show)

instance Arbitrary SchemeByteString where
  arbitrary = do
    let c = suchThat arbitrary isSchemeChar
    first <- suchThat arbitrary isAsciiAlpha
    len <- choose (0, 20)
    bs <- vectorOf len c
    return . SchemeByteString . B8.pack $ (first : bs ++ [':'])

newtype NonSchemeByteString = NonSchemeByteString { unNonSchemeByteString :: B.ByteString }
  deriving (Eq, Show)
  
data SchemeFailureMode 
  = NonAsciiFirstChar
  | InvalidSchemeChar
  | NoTrailingSemicolon

schemeFailureMode :: Int -> SchemeFailureMode
schemeFailureMode 1 = NonAsciiFirstChar
schemeFailureMode 2 = InvalidSchemeChar
schemeFailureMode 3 = NoTrailingSemicolon
schemeFailureMode _ = undefined

instance Arbitrary NonSchemeByteString where
  arbitrary = do
    failure <- fmap schemeFailureMode $ choose (1, 3)
    first <- case failure of
          NonAsciiFirstChar -> suchThat arbitrary (\x -> (not $ isAsciiAlpha x) && C.isAscii x)
          _                 -> suchThat arbitrary isAsciiAlpha
    bs <- case failure of
          InvalidSchemeChar -> do
            let c = suchThat arbitrary (\x -> (not $ isSchemeChar x) && x /= ':' && C.isAscii x)
            len <- choose (5, 5)
            vectorOf len c
          NonAsciiFirstChar -> do
            let c = suchThat arbitrary isSchemeChar
            len <- choose (15, 15)
            vectorOf len c
          NoTrailingSemicolon -> do
            let c = suchThat arbitrary isSchemeChar
            len <- choose (10, 10)
            vectorOf len c

    let built = case failure of
          NoTrailingSemicolon -> first : bs
          _                   -> first : bs ++ [':']
    return . NonSchemeByteString . B8.pack $ built
    
