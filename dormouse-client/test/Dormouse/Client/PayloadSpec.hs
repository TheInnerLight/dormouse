module Dormouse.Client.PayloadSpec
  ( spec
  ) where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Text.Encoding
import Dormouse.Client.Types
import Dormouse.Client.Generators.Text
import Dormouse.Client.Payload

import Streamly
import qualified Streamly.Prelude as S

import Data.Proxy

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Range as Range
import qualified Streamly.External.ByteString as SEB

spec :: Spec
spec = before setup $ do
  describe "decodeText" $ do
    it "successfully decodes latin-1 text for iso-8859-1 charset" $ \sizeRanges -> do
      hedgehog $ do
        txt <- forAll . genLatin1BS $ sizeRanges
        let 
          resp = HttpResponse 
            { responseStatusCode = 200
            , responseHeaders = Map.fromList [("Content-Type", "text/plain; charset=iso-8859-1")]
            , responseBody = S.unfold SEB.read txt
            }
          expectedLatin1Text = decodeLatin1 txt
        resp' <- liftIO . deserialiseRequest html $ resp
        responseBody resp' === expectedLatin1Text
    it "successfully decodes utf8 text for utf8 charset" $ \sizeRanges -> do
      hedgehog $ do
        txt <- forAll . genUtf8BS $ sizeRanges
        let 
          resp = HttpResponse 
            { responseStatusCode = 200
            , responseHeaders = Map.fromList [("Content-Type", "text/plain; charset=utf8")]
            , responseBody = S.unfold SEB.read txt
            }
          expectedUtf8Text = decodeUtf8 txt
        resp' <- liftIO . deserialiseRequest html $ resp
        responseBody resp' === expectedUtf8Text
    it "successfully decodes utf8 text if no explicit charset applied" $ \sizeRanges -> do
      hedgehog $ do
        txt <- forAll . genUtf8BS $ sizeRanges
        let 
          resp = HttpResponse 
            { responseStatusCode = 200
            , responseHeaders = Map.empty
            , responseBody = S.unfold SEB.read txt
            }
          expectedUtf8Text = decodeUtf8 txt
        resp' <- liftIO . deserialiseRequest html $ resp
        responseBody resp' === expectedUtf8Text
  where
    setup = do
      return $ Range.exponential 0 100000
