
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
import Data.Typeable (cast)
import Dormouse.Headers
import Dormouse.Methods
import Dormouse.Url.Class
import qualified Data.ByteString as SB
import qualified Data.Map.Strict as Map

-- | Model of an HTTP request with type parameters: @scheme@ describing the uri scheme, @body@ describing the type of the content body, @contentTag@ describing the type, @method@
-- describing the HTTP verb associated with the request, @contentTag@ describing the type of content being sen and @acceptTag@ describing the type of content desired
data HttpRequest url method body contentTag acceptTag = IsUrl url => HttpRequest 
  { requestMethod :: !(HttpMethod method)
  , requestUri :: !url
  , requestHeaders :: Map.Map HeaderName SB.ByteString
  , requestBody :: body
  }

instance Eq body => Eq (HttpRequest url method body contentTag acceptTag) where
  (==) (HttpRequest { requestMethod = rm1, requestUri = ru1, requestHeaders = rh1, requestBody = rb1 }) (HttpRequest { requestMethod = rm2, requestUri = ru2, requestHeaders = rh2, requestBody = rb2 }) =
    rm1 == rm2 && ru1 == ru2 && rh1 == rh2 && rb1 == rb2

instance Show body => Show (HttpRequest url method body contentTag acceptTag) where
  show (HttpRequest { requestMethod = rm, requestUri = ru, requestHeaders = rh, requestBody = rb }) = 
    unlines
        [ "HttpRequest"
        , "{ requestMethod  = " ++ show rm
        , ", requestUri     = " ++ show ru
        , ", requestHeaders = " ++ show rh
        , ", requestBody    = " ++ show rb
        , "}"
        ]

instance HasHeaders (HttpRequest url method body contentTag acceptTag) where
  getHeaders = requestHeaders
  getHeaderValue key = Map.lookup key . requestHeaders

-- | Model of an HTTP response with the type parameter @body@ describing the type of the content body.
data HttpResponse body = HttpResponse
  { responseStatusCode :: !Int
  , responseHeaders :: Map.Map HeaderName SB.ByteString
  , responseBody :: body
  } deriving (Eq)

instance Show body => Show (HttpResponse body) where
  show (HttpResponse { responseStatusCode = rsc, responseHeaders = rh, responseBody = rb }) = 
    unlines
        [ "HttpResponse"
        , "{ responseStatusCode = " ++ show rsc
        , ", responseHeaders    = " ++ show rh
        , ", responseBody       = " ++ show rb
        , "}"
        ]

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
