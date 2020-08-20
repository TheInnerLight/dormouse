
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
import qualified Data.Map.Strict as Map

-- | Model of an HTTP request with type parameters: @scheme@ describing the uri scheme, @body@ describing the type of the content body, @contentTag@ describing the type, @method@
-- describing the HTTP verb associated with the request, @contentTag@ describing the type of content being sen and @acceptTag@ describing the type of content desired
data HttpRequest scheme method body contentTag acceptTag = HttpRequest 
  { requestMethod :: !(HttpMethod method)
  , requestUri :: !(Uri Absolute scheme)
  , requestHeaders :: Map.Map HeaderName SB.ByteString
  , requestBody :: body
  } deriving (Eq, Show)

instance HasHeaders (HttpRequest scheme method body contentTag acceptTag) where
  getHeaders = requestHeaders
  getHeaderValue key = Map.lookup key . requestHeaders

-- | Model of an HTTP response with the type parameter @body@ describing the type of the content body.
data HttpResponse body = HttpResponse
  { responseStatusCode :: !Int
  , responseHeaders :: Map.Map HeaderName SB.ByteString
  , responseBody :: body
  } deriving (Show, Eq)

instance HasHeaders (HttpResponse b) where
  getHeaders = responseHeaders
  getHeaderValue key = Map.lookup key . responseHeaders

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
