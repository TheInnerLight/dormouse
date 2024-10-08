{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.Client.MonadIOImpl
  ( sendHttp
  , genClientRequestFromUrlComponents
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function ((&))
import Data.Functor (($>))
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.ByteString as B
import Dormouse.Client.Class
import Dormouse.Client.Methods
import Dormouse.Client.Payload
import Dormouse.Client.Types
import Dormouse.Uri
import Dormouse.Uri.Encode
import Dormouse.Url
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Types.Status as NC
import qualified Streamly.External.ByteString as SEB
import qualified Streamly.Data.Stream as Stream

givesPopper :: Stream.Stream IO Word8 -> C.GivesPopper ()
givesPopper rawStream k = do
  let initialStream = Stream.chunksOf 32768 rawStream
  streamState <- newIORef initialStream
  let popper = do
        stream <- readIORef streamState
        test <- Stream.uncons stream
        case test of
          Just (elems, stream') -> writeIORef streamState stream' $> SEB.fromArray elems
          Nothing               -> return B.empty
  k popper

translateRequestBody :: RawRequestPayload -> C.RequestBody
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
        HttpScheme -> (False, fromMaybe 80 $ authorityPort authority)
        HttpsScheme -> (True, fromMaybe 443 $  authorityPort authority)
      queryText = fromMaybe "" queryParams in
  C.defaultRequest
    { C.host = host
    , C.path = encodePath path
    , C.secure = isSecure
    , C.port = fromIntegral port
    , C.queryString = encodeQuery queryText
    }

responseStream :: C.Response C.BodyReader -> Stream.Stream IO Word8
responseStream resp = 
    Stream.repeatM (C.brRead $ C.responseBody resp)
  & Stream.takeWhile (not . B.null)
  & Stream.concatMap (Stream.unfold SEB.reader)

sendHttp :: (HasDormouseClientConfig env, MonadReader env m, MonadIO m, IsUrl url) => HttpRequest url method RawRequestPayload contentTag acceptTag -> (HttpResponse (Stream.Stream IO Word8) -> IO (HttpResponse b)) -> m (HttpResponse b)
sendHttp HttpRequest { requestMethod = method, requestUrl = url, requestBody = reqBody, requestHeaders = reqHeaders} deserialiseResp = do
  manager <- clientManager <$> reader getDormouseClientConfig
  let initialRequest = genClientRequestFromUrlComponents $ asAnyUrl url
  let request = initialRequest { C.method = methodAsByteString method, C.requestBody = translateRequestBody reqBody, C.requestHeaders = Map.toList reqHeaders }
  liftIO $ C.withResponse request manager (\resp -> do
      let respHeaders = Map.fromList $ C.responseHeaders resp
      let statusCode = NC.statusCode . C.responseStatus $ resp
      deserialiseResp $ HttpResponse 
        { responseStatusCode = statusCode
        , responseHeaders = respHeaders
        , responseBody = responseStream resp
        }
      ) 
