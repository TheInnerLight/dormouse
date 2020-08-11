{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Dormouse.Types
  ( HttpRequest(..)
  , HttpResponse(..)
  , SomeDormouseException(..)
  , UnexpectedStatusCode(..)
  ) where

import Control.Exception.Safe (Exception(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Dormouse.Payload
import Dormouse.Headers
import Dormouse.Methods
import Dormouse.Uri.Types 
import qualified Data.ByteString as SB


data HttpRequest scheme method tag acceptTag = HttpPayload tag => HttpRequest 
  { method :: HttpMethod method
  , url :: Uri Absolute scheme
  , headers :: [(HeaderName, SB.ByteString)]
  , body :: RawReqPayload tag
  }

instance (Show (RawReqPayload tag), HttpPayload tag) => Show (HttpRequest scheme method tag acceptTag) where
  show (HttpRequest {method = method, url = url, headers = headers, body = body}) = 
    "Http Request { method: " <> show method <> " url:" <> show url <> " " <> "headers: " <> show headers <> " body: " <> show body <> " }"

instance Eq (RawReqPayload tag) => Eq (HttpRequest scheme method tag acceptTag) where
  (==) (HttpRequest {method = method1, url = url1, headers = headers1, body = body1}) (HttpRequest {method = method2, url = url2, headers = headers2, body = body2}) =
    methodAsByteString method1 == methodAsByteString method2 && url1 == url2 && headers1 == headers2 && body1 == body2

data HttpResponse tag = HttpPayload tag =>  HttpResponse
  { statusCode :: Int
  , headers :: [(HeaderName, SB.ByteString)]
  , body :: RawRespPayload tag
  }

instance Show (RawRespPayload tag) => Show (HttpResponse tag) where
  show (HttpResponse {statusCode = statusCode, headers = headers, body = rawBody}) = 
    "Http Response { statusCode: " <> show statusCode <> " headers: " <> show headers <> " body: " <> show rawBody <> " }"


data SomeDormouseException = forall e . Exception e => SomeDormouseException e

instance Show SomeDormouseException where
    show (SomeDormouseException e) = show e

instance Exception SomeDormouseException

data UnexpectedStatusCode = UnexpectedStatusCode { uscStatusCode :: Int }

instance Show (UnexpectedStatusCode) where
  show (UnexpectedStatusCode { uscStatusCode = statusCode }) = "Server returned unexpected status code: " <> show statusCode

instance Exception (UnexpectedStatusCode) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a


