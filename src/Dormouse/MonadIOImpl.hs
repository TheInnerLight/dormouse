{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.MonadIOImpl
  ( sendHttp
  , genClientRequestFromUrlComponents
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.ByteString as B
import Dormouse.Class
import Dormouse.Methods
import Dormouse.Payload
import Dormouse.Status
import Dormouse.Types
import Dormouse.Uri
import Dormouse.Uri.Encode
import Dormouse.Url
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Types.Status as NC
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString as SEB
import Streamly.Internal.Memory.Array.Types (Array(..))

givesPopper :: SerialT IO (Array Word8) -> C.GivesPopper ()
givesPopper initialStream k = do
  streamState <- newIORef initialStream
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

genClientRequestFromUrlComponents :: AnyUrl -> C.Request
genClientRequestFromUrlComponents url =
  let (scheme, comps) = case url of
        AnyUrl (HttpUrl uc)  -> (HttpScheme, uc)
        AnyUrl (HttpsUrl uc) -> (HttpsScheme, uc)
      authority = urlAuthority comps
      path = urlPath comps
      queryParams = urlQuery comps
      host = T.urlEncode False . encodeUtf8 . unHost . authorityHost $ authority
      (isSecure, port) = case scheme of
        HttpScheme -> (False, maybe 80 id $ authorityPort authority)
        HttpsScheme -> (True, maybe 443 id $  authorityPort authority)
      queryText = maybe "" (id) $ queryParams in
  C.defaultRequest
    { C.host = host
    , C.path = encodePath path
    , C.secure = isSecure
    , C.port = fromIntegral port
    , C.queryString = encodeQuery queryText
    }

sendHttp :: (HasDormouseConfig env, MonadReader env m, MonadIO m, MonadThrow m) => HttpRequest url method a contentTag acceptTag -> (a -> RequestPayload) -> (SerialT IO (Array Word8) -> IO b) -> m (HttpResponse b)
sendHttp HttpRequest { requestMethod = method, requestUri = url, requestBody = body, requestHeaders = headers} requestWriter responseBuilder = do
  manager <- fmap clientManager $ reader (getDormouseConfig)
  let initialRequest = genClientRequestFromUrlComponents $ asAnyUrl url
  let requestPayload = requestWriter body
  let request = initialRequest { C.method = methodAsByteString method, C.requestBody = translateRequestBody requestPayload, C.requestHeaders = Map.toList headers }
  response <- liftIO $ C.withResponse request manager (\resp -> do
      let serialBodyStream :: SerialT (IO) (Array Word8) = S.map (\(b,_) -> b) $ S.takeWhile (\(_, l) -> l > 0 ) $ S.map (\bs -> (SEB.toArray bs, B.length bs))  $ S.repeatM (C.brRead $ C.responseBody resp )
      blug <- responseBuilder serialBodyStream
      return $ resp { C.responseBody = blug }
      ) 
  let resp = HttpResponse 
       { responseStatusCode = NC.statusCode . C.responseStatus $ response
       , responseHeaders = Map.fromList $ C.responseHeaders response
       , responseBody = C.responseBody response
       }
  case responseStatusCode resp of
    Successful -> return resp
    _          -> throw $ UnexpectedStatusCode (responseStatusCode resp)
