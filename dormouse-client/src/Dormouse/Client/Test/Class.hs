{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 
-- This module is useful for testing by providing a concrete ByteString typed version of 'MonadDormouseClient' called `MonadDormouseTestClient`.  
--
-- The assumption is that, in most test cases, you probably want to verify the byte payload of the request (which you simply extract
-- from the request here as a @ByteString@) and provide a byte payload (also as a @ByteString@) in the response so that you can verify 
-- your repsonse payload can be decoded directly.
--
-- An implementation of `MonadDormouseTestClient` can be written in terms of either Strict or Lazy Bytestrings at your convenient and the other 
-- will be automatically provided for you.
--
-- The machinery in here uses orphan instances of 'MonadDormouseClient' so you should use this carefully and restrict this module to test 
-- cases only.
module Dormouse.Client.Test.Class
  ( MonadDormouseTestClient(..)
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Data.ByteString  as SB
import qualified Data.ByteString.Lazy  as LB
import Data.Word ( Word8 )
import Dormouse.Client.Class ( MonadDormouseClient(..) )
import Dormouse.Client.Payload ( RawRequestPayload(..) )
import Dormouse.Client.Types ( HttpRequest(..), HttpResponse(..) )
import Dormouse.Url ( IsUrl )
import Streamly ( SerialT )
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString as SEB
import qualified Streamly.External.ByteString.Lazy as SEBL

-- | MonadDormouseTestClient describes the capability to send and receive specifically ByteString typed HTTP Requests and Responses
class Monad m => MonadDormouseTestClient m where
  -- | Make the supplied HTTP request, expecting an HTTP response with a Lazy ByteString body to be delivered in some @MonadDormouseTest m@
  expectLbs :: IsUrl url => HttpRequest url method LB.ByteString contentTag acceptTag -> m (HttpResponse LB.ByteString)
  expectLbs req = do
    resp <- expectBs $ req {requestBody = LB.toStrict $ requestBody req}
    return $ resp {responseBody = LB.fromStrict $ responseBody resp}
  -- | Make the supplied HTTP request, expecting an HTTP response with a Strict ByteString body to be delivered in some @MonadDormouseTest m@
  expectBs :: IsUrl url => HttpRequest url method SB.ByteString contentTag acceptTag -> m (HttpResponse SB.ByteString)
  expectBs req = do
    resp <- expectLbs $ req {requestBody = LB.fromStrict $ requestBody req}
    return $ resp {responseBody = LB.toStrict $ responseBody resp}
  {-# MINIMAL expectLbs | expectBs #-}

instance (Monad m, MonadIO m, MonadDormouseTestClient m) => MonadDormouseClient m where
  send req deserialiseResp = do
    reqBody <- liftIO . S.fold SEB.write . extricateRequestStream . requestBody $ req
    let reqBs = req {requestBody = reqBody}
    respBs <- expectBs reqBs
    let respStream = S.unfold SEBL.read . LB.fromStrict $ responseBody respBs
    liftIO $ deserialiseResp $ respBs { responseBody = respStream }
    where 
      extricateRequestStream :: RawRequestPayload -> SerialT IO Word8
      extricateRequestStream (DefinedContentLength _ s) = s
      extricateRequestStream (ChunkedTransfer s) = s

