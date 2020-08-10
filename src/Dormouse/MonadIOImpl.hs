{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.MonadIOImpl
  ( UsingNetworkHttpClient
  , sendHttp
  ) where

import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Kind (Constraint)
import Data.Typeable (Typeable, cast)
import Dormouse.Backend
import Dormouse.Class
import Dormouse.Methods
import Dormouse.Payload
import Dormouse.Status
import Dormouse.Types
import Dormouse.Uri
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as NC

type UsingNetworkHttpClient tag acceptTag = (Typeable acceptTag, (RequestBackend (RawPayload tag)), ResponseBackend (RawPayload acceptTag))

sendHttp :: (HasDormouseConfig env, MonadReader env m, MonadIO m, MonadThrow m, UsingNetworkHttpClient tag acceptTag, HttpPayload acceptTag) => HttpRequest scheme method tag acceptTag -> m (HttpResponse acceptTag)
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
