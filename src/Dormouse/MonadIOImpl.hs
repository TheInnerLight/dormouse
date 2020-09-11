{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.MonadIOImpl
  ( sendHttp
  , genClientRequestFromUrlComponents
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function ((&))
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.ByteString as B
import Dormouse.Class
import Dormouse.Exception (UnexpectedStatusCode(..))
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
import qualified Streamly.Internal.Memory.ArrayStream as SIMA

givesPopper :: SerialT IO (Word8) -> C.GivesPopper ()
givesPopper rawStream k = do
  let initialStream = SIMA.arraysOf 32768 rawStream
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

responseStream :: C.Response C.BodyReader -> SerialT IO Word8
responseStream resp = 
    S.repeatM (C.brRead $ C.responseBody resp)
  & S.takeWhile (not . B.null)
  & S.concatMap (S.unfold SEB.read)

sendHttp :: (HasDormouseConfig env, MonadReader env m, MonadIO m, MonadThrow m) => HttpRequest url method RequestPayload contentTag acceptTag -> (HttpResponse (SerialT IO Word8) -> IO (HttpResponse b)) -> m (HttpResponse b)
sendHttp HttpRequest { requestMethod = method, requestUri = url, requestBody = reqBody, requestHeaders = reqHeaders} deserialiseResp = do
  manager <- fmap clientManager $ reader (getDormouseConfig)
  let initialRequest = genClientRequestFromUrlComponents $ asAnyUrl url
  let request = initialRequest { C.method = methodAsByteString method, C.requestBody = translateRequestBody reqBody, C.requestHeaders = Map.toList reqHeaders }
  response <- liftIO $ C.withResponse request manager (\resp -> do
      let respHeaders = Map.fromList $ C.responseHeaders resp
      let statusCode = NC.statusCode . C.responseStatus $ resp
      deserialiseResp $ HttpResponse 
        { responseStatusCode = statusCode
        , responseHeaders = respHeaders
        , responseBody = responseStream resp
        }
      ) 
  case responseStatusCode response of
    Successful -> return response
    _          -> throw $ UnexpectedStatusCode (responseStatusCode response)
