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
  , AllowedBody(..)
  , methodAsByteString
  , HasAcceptHeader(..)
  , HasContentType(..)
  , EmptyPayload(..)
  , HttpPayload(..)
  , DecodingException(..)
  , JsonLbsPayload
  , UrlFormPayload
  , RequestPayload(..)
  , json
  , urlForm
  , noBody
  , pattern Informational
  , pattern Successful
  , pattern Redirect
  , pattern ClientError
  , pattern ServerError
  , ensureHttp
  , ensureHttps
  , parseUri
  , parseHttpUrl
  , parseHttpsUrl
  , IsQueryVal(..)
  , Uri(..)
  , Url
  , AnyUrl(..)
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Kind (Constraint)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable, cast)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import Data.Text (Text)
import Dormouse.Class
import Dormouse.Headers
import Dormouse.Payload
import Dormouse.Methods
import Dormouse.Status
import Dormouse.Types
import Dormouse.Uri
import Dormouse.Uri.Types
import qualified Dormouse.MonadIOImpl as IOImpl
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as NC

-- | Create an HTTP request with the supplied URI and supplied method, containing no body and no headers
makeRequest :: (RequestPayloadConstraint EmptyPayload (), HttpPayload EmptyPayload) => HttpMethod method -> Url scheme -> HttpRequest scheme method () EmptyPayload acceptTag
makeRequest method uri = HttpRequest 
  { requestMethod = method
  , requestUri = uri
  , requestHeaders = Map.empty
  , requestBody = ()
  }

-- | Create an HTTP DELETE request with the supplied URI, containing no body and no headers
delete :: Url scheme -> HttpRequest scheme "DELETE" () EmptyPayload acceptTag
delete = makeRequest DELETE

-- | Create an HTTP GET request with the supplied URI, containing no body and no headers
get :: Url scheme -> HttpRequest scheme "GET" () EmptyPayload acceptTag
get = makeRequest GET

-- | Create an HTTP HEAD request with the supplied URI, containing no body and no headers
head :: Url scheme -> HttpRequest scheme "HEAD" () EmptyPayload acceptTag
head = makeRequest HEAD

-- | Create an HTTP PATCH request with the supplied URI, containing no body and no headers
patch :: Url scheme -> HttpRequest scheme "PATCH" () EmptyPayload acceptTag
patch = makeRequest PATCH

-- | Create an HTTP POST request with the supplied URI, containing no body and no headers
post :: Url scheme -> HttpRequest scheme "POST" () EmptyPayload acceptTag
post = makeRequest POST

-- | Create an HTTP PUT request with the supplied URI, containing no body and no headers
put :: Url scheme -> HttpRequest scheme "PUT" () EmptyPayload acceptTag
put = makeRequest PUT

-- | Supply a body to an HTTP request using the supplied tag to indicate how the request should be encoded
supplyBody :: (AllowedBody method b, HttpPayload contentTag) => Proxy contentTag -> b -> HttpRequest scheme method b' contentTag' acceptTag -> HttpRequest scheme method b contentTag acceptTag
supplyBody prox b (r@HttpRequest { requestHeaders = headers, requestBody = _, ..}) =
  HttpRequest 
    { requestHeaders = foldMap (\v -> Map.insert ("Content-Type" :: HeaderName) v headers) $ contentType prox
    , requestBody = b
    , ..
    }

-- | Supply a header to an HTTP request
supplyHeader :: (HeaderName, B.ByteString) -> HttpRequest scheme method b contentTag acceptTag -> HttpRequest scheme method b contentTag acceptTag
supplyHeader (k, v) r = r { requestHeaders = Map.insert k v $ requestHeaders r }

-- | Apply an accept header derived from the supplied tag proxy and add a type hint to the request, indicating how the response should be decodable
accept :: (HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest scheme method b' contentTag acceptTag -> HttpRequest scheme method b' contentTag acceptTag
accept prox r = maybe r (\v -> supplyHeader ("Accept", v) r) $ acceptHeader prox 

-- | Make the supplied HTTP request, expecting an HTTP response with body type `b' to be delivered in some 'MonadDormouse m'
expect :: (RequestPayloadConstraint contentTag b, ResponsePayloadConstraint acceptTag b', MonadDormouse m, HttpPayload contentTag, HttpPayload acceptTag) => HttpRequest scheme method b contentTag acceptTag -> m (HttpResponse b')
expect r = expectAs (proxyOfReq r) r
  where 
    proxyOfReq :: HttpRequest scheme method b contentTag acceptTag -> Proxy acceptTag
    proxyOfReq _ = Proxy

-- | Make the supplied HTTP request, expecting an HTTP response in the supplied format with body type `b' to be delivered in some 'MonadDormouse m'
expectAs :: (RequestPayloadConstraint contentTag b, ResponsePayloadConstraint acceptTag b', MonadDormouse m, HttpPayload contentTag, HttpPayload acceptTag) => Proxy acceptTag -> HttpRequest scheme method b contentTag acceptTag -> m (HttpResponse b')
expectAs tag r = do
  resp <- send r (createRequestPayload (contentTypeProx r)) (extractResponsePayload tag)
  return resp
  where 
    contentTypeProx :: HttpRequest scheme method b contentTag acceptTag -> Proxy contentTag
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
