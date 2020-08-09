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
  , DormouseT
  , Dormouse
  , AllowedBody
  , UsingNetworkHttpClient
  , delete
  , get
  , Dormouse.head
  , patch
  , post
  , put
  , sendHttp
  , supplyBody
  , decodeBody
  , decodeBodyAs
  , accept
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
import GHC.TypeLits
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as NC
import URI.ByteString (URI, URIRef(..), Absolute, Host(..), Scheme(..), Port(..), Authority(..), Query(..))

emptyPayloadReq :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => HttpMethod method -> URI -> HttpRequest method EmptyPayload acceptTag
emptyPayloadReq method url = HttpRequest 
  { method = method
  , url = url
  , headers = []
  , body = LB.empty
  }

delete :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => URI -> HttpRequest "DELETE" EmptyPayload acceptTag
delete = emptyPayloadReq DELETE

get :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => URI -> HttpRequest "GET" EmptyPayload acceptTag
get = emptyPayloadReq GET

head :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => URI -> HttpRequest "HEAD" EmptyPayload acceptTag
head = emptyPayloadReq HEAD

patch :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => URI -> HttpRequest "PATCH" EmptyPayload acceptTag
patch = emptyPayloadReq PATCH

post :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => URI -> HttpRequest "POST" EmptyPayload acceptTag
post = emptyPayloadReq POST

put :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => URI -> HttpRequest "PUT" EmptyPayload acceptTag
put = emptyPayloadReq PUT

type family AllowedBody (a :: Symbol) b :: Constraint

type instance AllowedBody "CONNECT" b = (b ~ ())
type instance AllowedBody "DELETE" b = ()
type instance AllowedBody "GET" b = (b ~ ())
type instance AllowedBody "HEAD" b = (b ~ ())
type instance AllowedBody "OPTIONS" b = (b ~ ())
type instance AllowedBody "PATCH" b = ()
type instance AllowedBody "POST" b = ()
type instance AllowedBody "PUT" b = ()
type instance AllowedBody "TRACE" b = (b ~ ())

supplyBody :: (RequestPayloadConstraint tag b, HttpPayload tag, AllowedBody method b) => Proxy tag -> b -> HttpRequest method tag' acceptTag -> HttpRequest method tag acceptTag
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

accept :: (HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest method tag acceptTag -> HttpRequest method tag acceptTag
accept prox (h@HttpRequest { headers = oldHeaders}) = h { headers = oldHeaders <> newHeaders}
  where 
    newHeaders = foldMap (\v -> [("Accept" :: HeaderName, v)]) $ acceptHeader prox

type UsingNetworkHttpClient tag acceptTag = (HttpPayload acceptTag, Typeable acceptTag, (RequestBackend (RawPayload tag)), ResponseBackend (RawPayload acceptTag))

sendHttp :: (HasDormouseConfig env, MonadReader env m, MonadIO m, MonadThrow m, UsingNetworkHttpClient tag acceptTag) => HttpRequest method tag acceptTag -> m (HttpResponse acceptTag)
sendHttp HttpRequest {method = method, url = url, body = rawBody, headers = headers} = do
  manager <- fmap clientManager $ reader (getDormouseConfig)
  initialRequest <- parseRequestFromUri url
  let request = initialRequest { C.method = methodAsByteString method, C.requestBody = writeResponseBody rawBody, C.requestHeaders = headers }
  response <- liftIO $ C.withResponse request manager readResponseBody
  let resp = HttpResponse 
       { statusCode = NC.statusCode . C.responseStatus $ response
       , headers = C.responseHeaders response
       , body = C.responseBody response
       }
  case statusCode resp of
    Successful -> return resp
    _          -> throw $ UnexpectedStatusCode (statusCode resp) resp

newtype DormouseT m a = DormouseT 
  { unDormouseT :: ReaderT DormouseConfig m a 
  } deriving (Functor, Applicative, Monad, MonadReader DormouseConfig, MonadIO, MonadThrow)

instance (MonadIO m, MonadThrow m) => MonadDormouse (DormouseT m) where
  type MonadHttpConstraint (DormouseT m) tag acceptTag = UsingNetworkHttpClient tag acceptTag
  send = sendHttp

type Dormouse a = DormouseT IO a

runDormouseT :: DormouseConfig -> DormouseT m a -> m a
runDormouseT config dormouseT = runReaderT (unDormouseT dormouseT) config

runDormouse :: DormouseConfig -> Dormouse a -> IO a
runDormouse = runDormouseT
