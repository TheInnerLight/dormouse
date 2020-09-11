{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Dormouse
  ( module Dormouse.Types
  , DormouseT
  , Dormouse
  , delete
  , get
  , Dormouse.head
  , patch
  , post
  , put
  , supplyBody
  , accept
  , expectAs
  , expect
  , runDormouseT
  , runDormouse
  , C.newManager
  , TLS.tlsManagerSettings
  , MonadDormouse(..)
  , HasDormouseConfig(..)
  , DormouseConfig(..)
  , HeaderName
  , HasHeaders(..)
  , HttpMethod(..)
  , AllowedBody
  , methodAsByteString
  , HasMediaType(..)
  , EmptyPayload(..)
  , HttpPayload(..)
  , SomeDormouseException(..)
  , DecodingException(..)
  , MediaTypeException(..)
  , UnexpectedStatusCode(..)
  , UriException(..)
  , UrlException(..)
  , JsonPayload
  , UrlFormPayload
  , RequestPayload(..)
  , json
  , urlForm
  , noPayload
  , html
  , ensureHttp
  , ensureHttps
  , parseUri
  , parseUrl
  , parseHttpUrl
  , parseHttpsUrl
  , QueryBuilder
  , IsQueryVal(..)
  , Uri
  , Url
  , AnyUrl(..)
  , IsUrl(..)
  , Empty
  ) where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.Proxy
import Dormouse.Class
import Dormouse.Data
import Dormouse.Exception
import Dormouse.Headers
import Dormouse.Headers.MediaType
import Dormouse.Payload
import Dormouse.Methods
import Dormouse.Types
import Dormouse.Uri
import Dormouse.Url
import qualified Dormouse.MonadIOImpl as IOImpl
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

-- | Create an HTTP request with the supplied URI and supplied method, containing no body and no headers
makeRequest :: (RequestPayloadConstraint EmptyPayload Empty, HttpPayload EmptyPayload, IsUrl url) => HttpMethod method -> url -> HttpRequest url method Empty EmptyPayload acceptTag
makeRequest method url = HttpRequest 
  { requestMethod = method
  , requestUri = url
  , requestHeaders = Map.empty
  , requestBody = Empty
  }

-- | Create an HTTP DELETE request with the supplied URI, containing no body and no headers
delete :: IsUrl url => url -> HttpRequest url "DELETE" Empty EmptyPayload acceptTag
delete = makeRequest DELETE

-- | Create an HTTP GET request with the supplied URI, containing no body and no headers
get :: IsUrl url => url  -> HttpRequest url "GET" Empty EmptyPayload acceptTag
get = makeRequest GET

-- | Create an HTTP HEAD request with the supplied URI, containing no body and no headers
head :: IsUrl url => url  -> HttpRequest url "HEAD" Empty EmptyPayload acceptTag
head = makeRequest HEAD

-- | Create an HTTP PATCH request with the supplied URI, containing no body and no headers
patch :: IsUrl url => url  -> HttpRequest url "PATCH" Empty EmptyPayload acceptTag
patch = makeRequest PATCH

-- | Create an HTTP POST request with the supplied URI, containing no body and no headers
post :: IsUrl url => url  -> HttpRequest url "POST" Empty EmptyPayload acceptTag
post = makeRequest POST

-- | Create an HTTP PUT request with the supplied URI, containing no body and no headers
put :: IsUrl url => url  -> HttpRequest url "PUT" Empty EmptyPayload acceptTag
put = makeRequest PUT

-- | Supply a body to an HTTP request using the supplied tag to indicate how the request should be encoded
supplyBody :: (AllowedBody method b, HttpPayload contentTag) => Proxy contentTag -> b -> HttpRequest url method b' contentTag' acceptTag -> HttpRequest url method b contentTag acceptTag
supplyBody prox b (HttpRequest { requestHeaders = headers, requestBody = _, ..}) =
  HttpRequest 
    { requestHeaders = foldMap (\v -> Map.insert ("Content-Type" :: HeaderName) v headers) . fmap mediaTypeAsByteString $ mediaType prox
    , requestBody = b
    , ..
    }

-- | Supply a header to an HTTP request
supplyHeader :: (HeaderName, B.ByteString) -> HttpRequest url method b contentTag acceptTag -> HttpRequest url method b contentTag acceptTag
supplyHeader (k, v) r = r { requestHeaders = Map.insert k v $ requestHeaders r }

-- | Apply an accept header derived from the supplied tag proxy and add a type hint to the request, indicating how the response should be decodable
accept :: (HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest url method b' contentTag acceptTag -> HttpRequest url method b' contentTag acceptTag
accept prox r = maybe r (\v -> supplyHeader ("Accept", v) r) . fmap mediaTypeAsByteString $ mediaType prox

-- | Make the supplied HTTP request, expecting an HTTP response with body type `b' to be delivered in some 'MonadDormouse m'
expect :: (RequestPayloadConstraint contentTag b, ResponsePayloadConstraint acceptTag b', MonadDormouse m, HttpPayload contentTag, HttpPayload acceptTag) => HttpRequest url method b contentTag acceptTag -> m (HttpResponse b')
expect r = expectAs (proxyOfReq r) r
  where 
    proxyOfReq :: HttpRequest url method b contentTag acceptTag -> Proxy acceptTag
    proxyOfReq _ = Proxy

-- | Make the supplied HTTP request, expecting an HTTP response in the supplied format with body type `b' to be delivered in some 'MonadDormouse m'
expectAs :: (RequestPayloadConstraint contentTag b, ResponsePayloadConstraint acceptTag b', MonadDormouse m, HttpPayload contentTag, HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest url method b contentTag acceptTag -> m (HttpResponse b')
expectAs tag r = do
  let r' = serialiseRequest (contentTypeProx r) r
  resp <- send r' $ deserialiseRequest tag
  return resp
  where 
    contentTypeProx :: HttpRequest url method b contentTag acceptTag -> Proxy contentTag
    contentTypeProx _ = Proxy

-- | The DormouseT Monad Transformer
newtype DormouseT m a = DormouseT 
  { unDormouseT :: ReaderT DormouseConfig m a 
  } deriving (Functor, Applicative, Monad, MonadReader DormouseConfig, MonadIO, MonadThrow, MonadTrans)

instance (MonadIO m, MonadThrow m) => MonadDormouse (DormouseT m) where
  send = IOImpl.sendHttp

-- | A simple monad that allows you to run Dormouse
type Dormouse a = DormouseT IO a

-- | Run a DormouseT using the supplied 'DormouseConfig' to generate a result in the underlying monad @m@
runDormouseT :: DormouseConfig -> DormouseT m a -> m a
runDormouseT config dormouseT = runReaderT (unDormouseT dormouseT) config

-- | Run a Dormouse using the supplied 'DormouseConfig' to generate a result in 'IO'
runDormouse :: DormouseConfig -> Dormouse a -> IO a
runDormouse = runDormouseT
