{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.MonadIOImpl
  ( sendHttp
  ) where

import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Kind (Constraint)
import Data.Typeable (Typeable, cast)
import Data.Word (Word8)
import Data.ByteString as B
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
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as SS
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Streamly.External.ByteString as Strict
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import qualified Streamly.External.ByteString as SEB
import qualified Streamly.External.ByteString.Lazy as SEBL
import Streamly.Internal.Memory.Array.Types (Array(..))

sendHttp :: (HasDormouseConfig env, MonadReader env m, MonadIO m, MonadThrow m, HttpPayload tag, HttpPayload acceptTag) => HttpRequest scheme method tag acceptTag -> (SerialT IO (Array Word8) -> IO b) -> m (HttpResponse b)
sendHttp HttpRequest {method = method, url = url, body = rawBody, headers = headers} responseBuilder = do
  manager <- fmap clientManager $ reader (getDormouseConfig)
  initialRequest <- parseRequestFromUri url
  let request = initialRequest { C.method = methodAsByteString method, C.requestBody = writeRequestBody rawBody, C.requestHeaders = headers }

  response <- liftIO $ C.withResponse request manager (\resp -> do
      let serialBodyStream :: SerialT (IO) (Array Word8) = S.map (\(b,_) -> b) $ S.takeWhile (\(_, l) -> l > 0 ) $ S.map (\bs -> (SEB.toArray bs, B.length bs))  $ S.repeatM (C.brRead $ C.responseBody resp )
      blug <- responseBuilder serialBodyStream
      return $ resp { C.responseBody = blug }
      ) 
    
  let resp = HttpResponse 
       { statusCode = NC.statusCode . C.responseStatus $ response
       , headers = C.responseHeaders response
       , body = C.responseBody response
       }
  case statusCode resp of
    Successful -> return resp
    _          -> throw $ UnexpectedStatusCode (statusCode resp)

