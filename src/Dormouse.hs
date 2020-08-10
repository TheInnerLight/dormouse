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
  , decodeBody
  , decodeBodyAs
  , accept
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
import Data.Aeson (FromJSON, ToJSON, Value, encode, fromEncoding, decode)
import Data.Kind (Constraint)
import Data.Typeable (Typeable, cast)
import qualified Data.ByteString as SB
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
import GHC.TypeLits
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

delete :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "DELETE" EmptyPayload acceptTag
delete = emptyPayloadReq DELETE

get :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "GET" EmptyPayload acceptTag
get = emptyPayloadReq GET

head :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "HEAD" EmptyPayload acceptTag
head = emptyPayloadReq HEAD

patch :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "PATCH" EmptyPayload acceptTag
patch = emptyPayloadReq PATCH

post :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "POST" EmptyPayload acceptTag
post = emptyPayloadReq POST

put :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => Uri Absolute scheme -> HttpRequest scheme "PUT" EmptyPayload acceptTag
put = emptyPayloadReq PUT

supplyBody :: (RequestPayloadConstraint tag b, HttpPayload tag, AllowedBody method b) => Proxy tag -> b -> HttpRequest scheme method tag' acceptTag -> HttpRequest scheme method tag acceptTag
supplyBody prox a (HttpRequest {headers = headers, body = _, ..}) = 
  HttpRequest 
    { headers = foldMap (\v -> [("Content-Type" :: HeaderName, v)]) $ contentType prox
    , body = createRequestPayload prox a
    , ..
    }

decodeBody :: (ResponsePayloadConstraint tag b, HttpPayload tag, MonadThrow m, RawPayload tag ~ rawBody) => HttpResponse tag -> m b
decodeBody (hr@HttpResponse {body = x}) = extractResponsePayload (proxyOfResp hr) x
  where 
    proxyOfResp :: HttpResponse tag -> Proxy tag
    proxyOfResp _ = Proxy

decodeBodyAs :: (ResponsePayloadConstraint tag b, HttpPayload tag, MonadThrow m, RawPayload tag ~ rawBody) => Proxy tag -> HttpResponse tag -> m b
decodeBodyAs proxy (hr@HttpResponse {body = x}) = extractResponsePayload proxy x

accept :: (HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest scheme method tag acceptTag -> HttpRequest scheme method tag acceptTag
accept prox (h@HttpRequest { headers = oldHeaders}) = h { headers = oldHeaders <> newHeaders}
  where 
    newHeaders = foldMap (\v -> [("Accept" :: HeaderName, v)]) $ acceptHeader prox

expect :: (MonadDormouse m, MonadHttpConstraint m tag acceptTag, HttpPayload acceptTag, MonadThrow m, ResponsePayloadConstraint acceptTag b) => HttpRequest scheme method tag acceptTag -> m b
expect r = do
  resp <- send r
  b <- decodeBody resp
  return b

newtype DormouseT m a = DormouseT 
  { unDormouseT :: ReaderT DormouseConfig m a 
  } deriving (Functor, Applicative, Monad, MonadReader DormouseConfig, MonadIO, MonadThrow)

instance (MonadIO m, MonadThrow m) => MonadDormouse (DormouseT m) where
  type MonadHttpConstraint (DormouseT m) tag acceptTag = IOImpl.UsingNetworkHttpClient tag acceptTag
  send = IOImpl.sendHttp

type Dormouse a = DormouseT IO a

runDormouseT :: DormouseConfig -> DormouseT m a -> m a
runDormouseT config dormouseT = runReaderT (unDormouseT dormouseT) config

runDormouse :: DormouseConfig -> Dormouse a -> IO a
runDormouse = runDormouseT
