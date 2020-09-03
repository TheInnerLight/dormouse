{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module DormouseSpec 
  ( tests
  ) where

import Control.Concurrent.MVar
import Control.Exception.Safe (MonadThrow, Exception(..), throwString)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (encode, Value)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Word (Word8)
import Dormouse
import Dormouse.Headers
import Dormouse.Url.QQ
import Dormouse.Generators.Json
import Streamly
import Streamly.Internal.Memory.Array.Types (Array(..))
import qualified Streamly.External.ByteString.Lazy as SEBL
import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data TestEnv = TestEnv 
  { sentJson :: MVar (Maybe (LB.ByteString))
  , sentContentType :: MVar (Maybe B.ByteString)
  , sentAcceptHeader :: MVar (Maybe B.ByteString)
  , returnJson :: MVar (Maybe (LB.ByteString))
  }

newtype TestM a = TestM { unTestM :: ReaderT TestEnv IO a} deriving (Functor, Applicative, Monad, MonadReader TestEnv, MonadIO, MonadThrow)

runTestM :: TestEnv -> TestM a -> IO a
runTestM deps app = flip runReaderT deps $ unTestM app

extricateRequestStream :: RequestPayload -> SerialT IO (Array Word8)
extricateRequestStream (DefinedContentLength _ s) = s
extricateRequestStream (ChunkedTransfer s) = s
instance MonadDormouse TestM where
  send req requestWriter responseBuilder = do
    testEnv <- ask
    reqLbs <- liftIO . SEBL.fromChunksIO . extricateRequestStream . requestWriter $ requestBody req
    _ <- liftIO $ swapMVar (sentContentType testEnv) . getHeaderValue "Content-Type" $ req
    _ <- liftIO $ swapMVar (sentAcceptHeader testEnv) . getHeaderValue "Accept" $ req
    _ <- liftIO $ swapMVar (sentJson testEnv) $ Just reqLbs
    maybeResponseJson <- liftIO . readMVar $ returnJson testEnv
    let respLbs = maybe (encode ()) id maybeResponseJson
    resp <- liftIO $ responseBuilder . SEBL.toChunks $ respLbs
    pure HttpResponse
      { responseStatusCode = 200
      , responseHeaders = Map.empty
      , responseBody = resp
      }

tests :: IO()
tests = do
  sentJson' <- newMVar Nothing
  sentContentType' <- newMVar Nothing
  sentAcceptHeader' <- newMVar Nothing
  returnJson' <- newMVar Nothing
  let testEnv = TestEnv 
        { sentJson = sentJson'
        , sentContentType = sentContentType'
        , sentAcceptHeader = sentAcceptHeader'
        , returnJson = returnJson'
        }
  let jsonGenRanges = JsonGenRanges 
        { stringRanges = Range.constant 0 15
        , doubleRanges = Range.constant (-100000) (1000000)
        , arrayLenRanges = Range.constant 0 7
        }
  hspec $ do
    describe "expect" $ do
      it "submits the correct json body, content-type and accept header for a json request" $ do
        hedgehog $ do
          arbJson <- forAll . genJsonValue $ jsonGenRanges
          let req = accept noPayload $ supplyBody json arbJson $ post [https|https://testing123.com|]
          _ :: HttpResponse Empty <- liftIO $ runTestM testEnv $ expect req
          lbs <- liftIO $ readMVar sentJson'
          actualContentType <- liftIO $ readMVar sentContentType'
          actualAcceptHeader <- liftIO $ readMVar sentAcceptHeader'
          let expected = encode arbJson
          lbs === (Just expected)
          actualContentType === Just "application/json"
          actualAcceptHeader === Nothing
      it "submits the correct accept header and gets the correct json body for a json response" $ do
        hedgehog $ do
          arbJson <- forAll . genJsonValue $ jsonGenRanges
          _ <- liftIO $ swapMVar returnJson' $ Just (encode arbJson)
          let req = accept json $ supplyBody json () $ post [https|https://testing123.com|]
          r :: HttpResponse Value <- liftIO $ runTestM testEnv $ expect req
          actualAcceptHeader <- liftIO $ readMVar sentAcceptHeader'
          let actualJson = responseBody r
          actualJson === arbJson
          actualAcceptHeader === Just "application/json"
    describe "expectAs" $ do
      it "submits the correct json body, content-type and accept header for a json request" $ do
        hedgehog $ do
          arbJson <- forAll . genJsonValue $ jsonGenRanges
          let req = supplyBody json arbJson $ post [https|https://testing123.com|]
          _ :: HttpResponse Empty <- liftIO $ runTestM testEnv $ expectAs noPayload req
          lbs <- liftIO $ readMVar sentJson'
          actualContentType <- liftIO $ readMVar sentContentType'
          actualAcceptHeader <- liftIO $ readMVar sentAcceptHeader'
          let expected = encode arbJson
          lbs === (Just expected)
          actualContentType === (Just "application/json")
          actualAcceptHeader === Nothing
      it "submits the correct accept header and gets the correct json body for a json response" $ do
        hedgehog $ do
          arbJson <- forAll . genJsonValue $ jsonGenRanges
          _ <- liftIO $ swapMVar returnJson' $ Just (encode arbJson)
          let req = supplyBody json () $ post [https|https://testing123.com|]
          r :: HttpResponse Value <- liftIO $ runTestM testEnv $ expectAs json req
          actualAcceptHeader <- liftIO $ readMVar sentAcceptHeader'
          let actualJson = responseBody r
          actualJson === arbJson
          actualAcceptHeader === Nothing
