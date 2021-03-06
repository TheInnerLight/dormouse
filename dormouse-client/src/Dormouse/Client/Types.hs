
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Dormouse.Client.Types
  ( HttpRequest(..)
  , HttpResponse(..)
  ) where

import Dormouse.Client.Headers
import Dormouse.Client.Methods
import qualified Data.ByteString as SB
import qualified Data.Map.Strict as Map

-- | Model of an HTTP request with type parameters: @scheme@ describing the uri scheme, @body@ describing the type of the content body, @contentTag@ describing the type, @method@
-- describing the HTTP verb associated with the request, @contentTag@ describing the type of content being sen and @acceptTag@ describing the type of content desired
data HttpRequest url method body contentTag acceptTag = HttpRequest 
  { requestMethod :: !(HttpMethod method)
  , requestUrl :: !url
  , requestHeaders :: Map.Map HeaderName SB.ByteString
  , requestBody :: body
  }

instance (Eq body, Eq url) => Eq (HttpRequest url method body contentTag acceptTag) where
  (==) (HttpRequest { requestMethod = rm1, requestUrl = ru1, requestHeaders = rh1, requestBody = rb1 }) (HttpRequest { requestMethod = rm2, requestUrl = ru2, requestHeaders = rh2, requestBody = rb2 }) =
    rm1 == rm2 && ru1 == ru2 && rh1 == rh2 && rb1 == rb2

instance (Show body, Show url) => Show (HttpRequest url method body contentTag acceptTag) where
  show (HttpRequest { requestMethod = rm, requestUrl = ru, requestHeaders = rh, requestBody = rb }) = 
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
