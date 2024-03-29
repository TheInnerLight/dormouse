{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | The "Client" module is the primary module you will need to import to perform HTTP requests with this library.
--
--  For a comprehensive documentation, please see: <https://dormouse.io/client.html>
module Dormouse.Client
  ( -- * Request / Response Types
    HttpRequest(..)
  , HttpResponse(..)
  -- * Request building
  , delete
  , get
  , Dormouse.Client.head
  , patch
  , post
  , put
  , supplyBody
  , accept
  , expectAs
  , expect
  , fetchAs
  , fetch
  -- * Dormouse Client Monad and Transformer
  , DormouseClientT
  , DormouseClient
  , runDormouseClientT
  , runDormouseClient
  -- * Dormouse Client Class
  , MonadDormouseClient(..)
  -- * Dormouse Client Config
  , C.newManager
  , TLS.tlsManagerSettings
  , HasDormouseClientConfig(..)
  , DormouseClientConfig(..)
  -- * Headers
  , HeaderName
  , HasHeaders(..)
  , HasMediaType(..)
  -- * Methods
  , HttpMethod(..)
  , AllowedBody
  , methodAsByteString
  -- * Payloads
  , RawRequestPayload(..)
  , RequestPayload(..)
  , ResponsePayload(..)
  , EmptyPayload
  , HtmlPayload
  , JsonPayload
  , UrlFormPayload
  , Empty
  , json
  , urlForm
  , noPayload
  , html
  -- * Exceptions
  , SomeDormouseException(..)
  , DecodingException(..)
  , MediaTypeException(..)
  , UnexpectedStatusCodeException(..)
  , UriException(..)
  , UrlException(..)
  -- * Uri
  , Uri
  , parseUri
  -- * Url
  , Url
  , AnyUrl(..)
  , IsUrl(..)
  , ensureHttp
  , ensureHttps
  , parseUrl
  , parseHttpUrl
  , parseHttpsUrl
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.Proxy
import Dormouse.Client.Class
import Dormouse.Client.Data
import Dormouse.Client.Exception
import Dormouse.Client.Headers
import Dormouse.Client.Headers.MediaType
import Dormouse.Client.Payload
import Dormouse.Client.Methods
import Dormouse.Client.Status
import Dormouse.Client.Types
import Dormouse.Uri
import Dormouse.Url
import qualified Dormouse.Client.MonadIOImpl as IOImpl
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

-- | Create an HTTP request with the supplied URI and supplied method, containing no body and no headers
makeRequest :: IsUrl url => HttpMethod method -> url -> HttpRequest url method Empty EmptyPayload acceptTag
makeRequest method url = HttpRequest 
  { requestMethod = method
  , requestUrl = url
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
supplyBody :: (AllowedBody method b, RequestPayload b contentTag) => Proxy contentTag -> b -> HttpRequest url method b' contentTag' acceptTag -> HttpRequest url method b contentTag acceptTag
supplyBody prox b HttpRequest { requestHeaders = headers, requestBody = _, ..} =
  HttpRequest 
    { requestHeaders = foldMap (\v -> Map.insert ("Content-Type" :: HeaderName) v headers) . fmap encodeMediaType $ mediaType prox
    , requestBody = b
    , ..
    }

-- | Supply a header to an HTTP request
supplyHeader :: (HeaderName, B.ByteString) -> HttpRequest url method b contentTag acceptTag -> HttpRequest url method b contentTag acceptTag
supplyHeader (k, v) r = r { requestHeaders = Map.insert k v $ requestHeaders r }

-- | Apply an accept header derived from the supplied tag proxy and add a type hint to the request, indicating how the response should be decodable
accept :: HasMediaType acceptTag => Proxy acceptTag -> HttpRequest url method b contentTag acceptTag -> HttpRequest url method b contentTag acceptTag
accept prox r = maybe r (\v -> supplyHeader ("Accept", v) r) . fmap encodeMediaType $ mediaType prox

-- | Make the supplied HTTP request, expecting a Successful (2xx) HTTP response with body type `b' to be delivered in some 'MonadDormouseClient m'
expect :: (MonadDormouseClient m, MonadThrow m, RequestPayload b contentTag, ResponsePayload b' acceptTag, IsUrl url) 
       => HttpRequest url method b contentTag acceptTag -> m (HttpResponse b')
expect r = expectAs (proxyOfReq r) r
  where 
    proxyOfReq :: HttpRequest url method b contentTag acceptTag -> Proxy acceptTag
    proxyOfReq _ = Proxy

-- | Make the supplied HTTP request, expecting a Successful (2xx) HTTP response in the supplied format with body type `b' to be delivered in some 'MonadDormouseClient m'
expectAs :: (MonadDormouseClient m, MonadThrow m, RequestPayload b contentTag, ResponsePayload b' acceptTag, IsUrl url) 
         => Proxy acceptTag -> HttpRequest url method b contentTag acceptTag -> m (HttpResponse b')
expectAs tag r = fetchAs tag r rejectNon2xx

-- | Make the supplied HTTP request and transform the response into a result in some 'MonadDormouseClient m'
fetch :: (MonadDormouseClient m, RequestPayload b contentTag, ResponsePayload b' acceptTag, IsUrl url) 
        => HttpRequest url method b contentTag acceptTag -> (HttpResponse b' -> m b'') -> m b''
fetch r = fetchAs (proxyOfReq r) r
  where  
    proxyOfReq :: HttpRequest url method b contentTag acceptTag -> Proxy acceptTag
    proxyOfReq _ = Proxy

-- | Make the supplied HTTP request and transform the response in the supplied format into a result in some 'MonadDormouseClient m'
fetchAs :: (MonadDormouseClient m, RequestPayload b contentTag, ResponsePayload b' acceptTag, IsUrl url) 
        => Proxy acceptTag -> HttpRequest url method b contentTag acceptTag -> (HttpResponse b' -> m b'') -> m b''
fetchAs tag r f = do
  let r' = serialiseRequest (contentTypeProx r) r
  resp <- send r' $ deserialiseRequest tag
  f resp
  where  
    contentTypeProx :: HttpRequest url method b contentTag acceptTag -> Proxy contentTag
    contentTypeProx _ = Proxy

rejectNon2xx :: MonadThrow m => HttpResponse body -> m (HttpResponse body)
rejectNon2xx r = case responseStatusCode r of
  Successful -> pure r
  _          -> throw $ UnexpectedStatusCodeException (responseStatusCode r)

-- | The DormouseClientT Monad Transformer
newtype DormouseClientT m a = DormouseClientT 
  { unDormouseClientT :: ReaderT DormouseClientConfig m a 
  } deriving (Functor, Applicative, Monad, MonadReader DormouseClientConfig, MonadIO, MonadThrow, MonadTrans)

instance (MonadIO m, MonadThrow m) => MonadDormouseClient (DormouseClientT m) where
  send = IOImpl.sendHttp

-- | A simple monad that allows you to run DormouseClient
type DormouseClient a = DormouseClientT IO a

-- | Run a 'DormouseClientT' using the supplied 'DormouseClientConfig' to generate a result in the underlying monad @m@
runDormouseClientT :: DormouseClientConfig -> DormouseClientT m a -> m a
runDormouseClientT config dormouseClientT = runReaderT (unDormouseClientT dormouseClientT) config

-- | Run a 'DormouseClient' using the supplied 'DormouseClientConfig' to generate a result in 'IO'
runDormouseClient :: DormouseClientConfig -> DormouseClient a -> IO a
runDormouseClient = runDormouseClientT
