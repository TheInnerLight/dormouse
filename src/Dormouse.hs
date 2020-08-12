{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Dormouse
  ( module Dormouse.Class
  , module Dormouse.Headers 
  , module Dormouse.Methods
  , module Dormouse.Payload
  , module Dormouse.Status
  , module Dormouse.Types
  , module Dormouse.Uri
  , DormouseT
  , Dormouse
  , delete
  , get
  , Dormouse.head
  , patch
  , post
  , put
  , supplyBody
--  , decodeBody
--  , decodeBodyAs
  , accept
  , expectAs
  , expect
  , runDormouseT
  , runDormouse
  , C.newManager
  , TLS.tlsManagerSettings
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Kind (Constraint)
import Data.Typeable (Typeable, cast)
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import Data.Text (Text)
import Dormouse.Backend
import Dormouse.Class
import Dormouse.Headers
import Dormouse.Payload
import Dormouse.Methods
import Dormouse.Status
import Dormouse.Types
import Dormouse.Uri
import qualified Dormouse.MonadIOImpl as IOImpl
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as NC

emptyPayloadReq :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => HttpMethod method -> Uri Absolute scheme -> HttpRequest scheme method EmptyPayload acceptTag
emptyPayloadReq method url = HttpRequest 
  { method = method
  , url = url
  , headers = []
  , body = LB.empty
  }

-- | Basic template for an HTTP DELETE request to a supplied URI, contains no body and no headers
delete :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "DELETE" EmptyPayload acceptTag
delete = emptyPayloadReq DELETE

-- | Basic template for an HTTP GET request to a supplied URI, contains no body and no headers
get :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "GET" EmptyPayload acceptTag
get = emptyPayloadReq GET

-- | Basic template for an HTTP HEAD request to a supplied URI, contains no body and no headers
head :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "HEAD" EmptyPayload acceptTag
head = emptyPayloadReq HEAD

-- | Basic template for an HTTP PATCH request to a supplied URI, contains no body and no headers
patch :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "PATCH" EmptyPayload acceptTag
patch = emptyPayloadReq PATCH

-- | Basic template for an HTTP POST request to a supplied URI, contains no body and no headers
post :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "POST" EmptyPayload acceptTag
post = emptyPayloadReq POST

-- | Basic template for an HTTP PUT request to a supplied URI, contains no body and no headers
put :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "PUT" EmptyPayload acceptTag
put = emptyPayloadReq PUT

-- | Supply a body to an HTTP request using the supplied tag to indicate how the request should be encoded
supplyBody :: (RequestPayloadConstraint tag b, HttpPayload tag, AllowedBody method b) => Proxy tag -> b -> HttpRequest scheme method tag' acceptTag -> HttpRequest scheme method tag acceptTag
supplyBody prox a (HttpRequest {headers = headers, body = _, ..}) = 
  HttpRequest 
    { headers = foldMap (\v -> [("Content-Type" :: HeaderName, v)]) $ contentType prox
    , body = createRequestPayload prox a
    , ..
    }

-- -- | Decode the body of an HTTP response to some type `b`
-- decodeBody :: (MonadIO m, MonadThrow m, ResponsePayloadConstraint tag b, HttpPayload tag) => HttpResponse -> m b
-- decodeBody (hr@HttpResponse {body = x}) = extractResponsePayload (proxyOfResp hr) x
--   where 
--     proxyOfResp :: HttpResponse -> Proxy tag
--     proxyOfResp _ = Proxy

-- | Decode the body of an HTTP response to some type `b` with a supplied tag indicating how the response should be decoded
--decodeBodyAs :: (MonadIO m, MonadThrow m, ResponsePayloadConstraint tag b, HttpPayload tag, RawRespPayload tag ~ rawBody) => Proxy tag -> HttpResponse -> m b
--decodeBodyAs proxy (hr@HttpResponse {body = x}) = extractResponsePayload proxy x

-- | Apply an accept header derived from the supplied tag proxy and add a type hint to the request, indicating how the response should be decodable
accept :: (HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest scheme method tag acceptTag -> HttpRequest scheme method tag acceptTag
accept prox (h@HttpRequest { headers = oldHeaders}) = h { headers = oldHeaders <> newHeaders}
  where 
    newHeaders = foldMap (\v -> [("Accept" :: HeaderName, v)]) $ acceptHeader prox

expect :: (ResponsePayloadConstraint acceptTag b, MonadDormouse m, HttpPayload tag, HttpPayload acceptTag, HttpPayload acceptTag) => HttpRequest scheme method tag acceptTag -> m (HttpResponse b)
expect r = expectAs (proxyOfReq r) r
  where 
    proxyOfReq :: HttpRequest scheme method tag acceptTag -> Proxy accep
    proxyOfReq _ = Proxy

expectAs :: (ResponsePayloadConstraint acceptTag b, MonadDormouse m, HttpPayload tag, HttpPayload acceptTag, HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest scheme method tag acceptTag -> m (HttpResponse b)
expectAs tag r = do
  resp <- send r (extractResponsePayload tag)
  return resp

-- | The DormouseT Monad Transformer
newtype DormouseT m a = DormouseT 
  { unDormouseT :: ReaderT DormouseConfig m a 
  } deriving (Functor, Applicative, Monad, MonadReader DormouseConfig, MonadIO, MonadThrow, MonadTrans)

instance (MonadIO m, MonadThrow m) => MonadDormouse (DormouseT m) where
  send = IOImpl.sendHttp

type Dormouse a = DormouseT IO a

-- | Run a DormouseT using the supplied 'DormouseConfig' to generate a result in the underlying monad 'm'
runDormouseT :: DormouseConfig -> DormouseT m a -> m a
runDormouseT config dormouseT = runReaderT (unDormouseT dormouseT) config

-- | Run a Dormouse using the supplied 'DormouseConfig' to generate a result in 'IO'
runDormouse :: DormouseConfig -> Dormouse a -> IO a
runDormouse = runDormouseT
