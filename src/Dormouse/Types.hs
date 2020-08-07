{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module Dormouse.Types
  ( HttpRequest(..)
  , HttpResponse(..)
  , SomeDormouseException(..)
  , UnexpectedStatusCode(..)
  , UriException(..)
  ) where

import Control.Exception.Safe (Exception(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Dormouse.Payload
import Dormouse.Headers
import Dormouse.Methods
import qualified Data.ByteString as SB
import URI.ByteString (URI)

data HttpRequest method tag acceptTag = HttpPayload tag => HttpRequest 
  { method :: HttpMethod method
  , url :: URI
  , headers :: [(HeaderName, SB.ByteString)]
  , body :: RawPayload tag
  }

instance (Show (RawPayload tag), HttpPayload tag) => Show (HttpRequest method tag acceptTag) where
  show (HttpRequest {method = method, url = url, headers = headers, body = body}) = 
    "Http Request { method: " <> show method <> " url:" <> show url <> " " <> "headers: " <> show headers <> " body: " <> show body <> " }"

instance Eq (RawPayload tag) => Eq (HttpRequest method tag acceptTag) where
  (==) (HttpRequest {method = method1, url = url1, headers = headers1, body = body1}) (HttpRequest {method = method2, url = url2, headers = headers2, body = body2}) =
    methodAsByteString method1 == methodAsByteString method2 && url1 == url2 && headers1 == headers2 && body1 == body2

data HttpResponse tag = HttpPayload tag =>  HttpResponse
  { statusCode :: Int
  , headers :: [(HeaderName, SB.ByteString)]
  , body :: RawPayload tag
  }

instance Show (RawPayload tag) => Show (HttpResponse tag) where
  show (HttpResponse {statusCode = statusCode, headers = headers, body = rawBody}) = 
    "Http Response { statusCode: " <> show statusCode <> " headers: " <> show headers <> " body: " <> show rawBody <> " }"


data SomeDormouseException = forall e . Exception e => SomeDormouseException e

instance Show SomeDormouseException where
    show (SomeDormouseException e) = show e

instance Exception SomeDormouseException

data UnexpectedStatusCode a = UnexpectedStatusCode { uscStatusCode :: Int, uscResponse :: HttpResponse a }

instance Show (UnexpectedStatusCode a) where
  show (UnexpectedStatusCode { uscStatusCode = statusCode, uscResponse = resp }) = "Server returned unexpected status code: " <> show statusCode

instance Typeable a => Exception (UnexpectedStatusCode a) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

data UriException = UriException {uriExceptionUri :: URI, uriExceptionMessage :: Text}

instance Show (UriException) where
  show (UriException { uriExceptionUri = message, uriExceptionMessage = uri }) = "Failed to parse uri: " <> show uri <> " message: " <> show message

instance Exception (UriException) where
  toException    = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a


