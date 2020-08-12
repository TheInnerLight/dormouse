{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.MonadIOImpl
  ( sendHttp
  ) where

import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Kind (Constraint)
import Data.Typeable (Typeable, cast)
import Data.Word (Word8)
import Data.ByteString as B
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
import qualified Streamly.External.ByteString as SEB
import Streamly.Internal.Memory.Array.Types (Array(..))

givesPopper :: SerialT IO (Array Word8) -> C.GivesPopper ()
givesPopper stream k = do
  streamState <- newIORef stream
  let popper = do
        stream <- readIORef streamState
        test <- S.uncons stream
        case test of
          Just (elems, stream') -> writeIORef streamState stream' *> (return $ SEB.fromArray elems)
          Nothing               -> return B.empty
  k popper

translateRequestBody :: RequestPayload -> C.RequestBody
translateRequestBody (DefinedContentLength size stream) = C.RequestBodyStream (fromIntegral size) (givesPopper stream)
translateRequestBody (ChunkedTransfer stream)           = C.RequestBodyStreamChunked (givesPopper stream)

sendHttp :: (HasDormouseConfig env, MonadReader env m, MonadIO m, MonadThrow m) => HttpRequest scheme method a contentTag acceptTag -> (a -> RequestPayload) -> (SerialT IO (Array Word8) -> IO b) -> m (HttpResponse b)
sendHttp HttpRequest { requestMethod = method, requestUri = uri, requestBody = body, requestHeaders = headers} requestWriter responseBuilder = do
  manager <- fmap clientManager $ reader (getDormouseConfig)
  initialRequest <- parseRequestFromUri uri
  let requestPayload = requestWriter body
  let request = initialRequest { C.method = methodAsByteString method, C.requestBody = translateRequestBody requestPayload, C.requestHeaders = headers }
  response <- liftIO $ C.withResponse request manager (\resp -> do
      let serialBodyStream :: SerialT (IO) (Array Word8) = S.map (\(b,_) -> b) $ S.takeWhile (\(_, l) -> l > 0 ) $ S.map (\bs -> (SEB.toArray bs, B.length bs))  $ S.repeatM (C.brRead $ C.responseBody resp )
      blug <- responseBuilder serialBodyStream
      return $ resp { C.responseBody = blug }
      ) 
  let resp = HttpResponse 
       { responseStatusCode = NC.statusCode . C.responseStatus $ response
       , responseHeaders = C.responseHeaders response
       , responseBody = C.responseBody response
       }
  case responseStatusCode resp of
    Successful -> return resp
    _          -> throw $ UnexpectedStatusCode (responseStatusCode resp)
