module Dormouse.Client.Generators.Text 
  ( genLatin1BS
  , genUtf8BS
  )
  where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Data.ByteString
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS8

genLatin1BS :: Range Int -> Gen ByteString
genLatin1BS range = do
  t <- Gen.text range Gen.latin1 
  pure . BS8.pack . T.unpack $ t

genUtf8BS :: Range Int -> Gen ByteString
genUtf8BS range = 
  Gen.utf8 range Gen.unicode
