{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Dormouse.Client.Payload
  ( HasMediaType(..)
  , EmptyPayload
  , RequestPayload(..)
  , ResponsePayload(..)
  , JsonPayload
  , UrlFormPayload
  , HtmlPayload
  , RawRequestPayload(..)
  , QProxy
  , json
  , urlForm
  , noPayload
  , html
  ) where


import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import Dormouse.Client.Data
import Dormouse.Client.Types
import Dormouse.Client.Exception (DecodingException(..))
import Dormouse.Client.Headers
import Dormouse.Client.Headers.MediaType
import qualified Dormouse.Client.Headers.MediaType as MTH
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Web.FormUrlEncoded as W
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString as SEB
import qualified Streamly.External.ByteString.Lazy as SEBL


data QProxy tag where
  QProxy :: QProxy tag
  QPOr :: Double -> QProxy a -> Maybe Double -> QProxy b -> QProxy (a,b)

class AcceptMediaType tag where
  acceptHeader :: QProxy tag -> Maybe MediaType
  canAccept :: MediaType -> prox tag -> Bool

-- | Describes an association between a type @tag@ and a specific Media Type
class HasMediaType tag where
  mediaType :: prox tag -> Maybe MediaType

-- | A raw HTTP Request payload consisting of a stream of bytes with either a defined Content Length or using Chunked Transfer Encoding
data RawRequestPayload
  -- | DefinedContentLength represents a payload where the size of the message is known in advance and the content length header can be computed
  = DefinedContentLength Word64 (SerialT IO Word8)
  -- | ChunkedTransfer represents a payload with indertiminate length, to be sent using chunked transfer encoding
  | ChunkedTransfer (SerialT IO Word8)

-- | RequestPayload relates a type of content and a payload tag used to describe that type to its byte stream representation and the constraints required to encode it
class HasMediaType contentTag => RequestPayload body contentTag where
  -- | Generates a the byte stream representation from the supplied content
  serialiseRequest :: prox contentTag -> HttpRequest url method body contentTag acceptTag  -> HttpRequest url method RawRequestPayload contentTag acceptTag 

-- | ResponsePayload relates a type of content and a payload tag used to describe that type  to its byte stream representation and the constraints required to decode it
class HasMediaType tag => ResponsePayload body tag where
  -- | Decodes the high level representation from the supplied byte stream
  deserialiseRequest :: prox tag -> HttpResponse (SerialT IO Word8) -> IO (HttpResponse body)

data JsonPayload = JsonPayload

instance HasMediaType JsonPayload where
  mediaType _ = Just applicationJson

instance (ToJSON body) => RequestPayload body JsonPayload where
  serialiseRequest _ r = 
    let b = requestBody r
        lbs = encode b
    in r { requestBody = DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs) }

instance (FromJSON body) => ResponsePayload body JsonPayload where
  deserialiseRequest _ resp = do
    let stream = responseBody resp
    bs <- S.fold SEB.write stream
    body <- either (throw . DecodingException . T.pack) return . eitherDecodeStrict $ bs
    return $ resp { responseBody = body }

-- | A type tag used to indicate that a request\/response should be encoded\/decoded as @application/json@ data
json :: QProxy JsonPayload
json = QProxy

data UrlFormPayload = UrlFormPayload

instance HasMediaType UrlFormPayload where
  mediaType _ = Just applicationXWWWFormUrlEncoded

instance (W.ToForm body) => RequestPayload body UrlFormPayload where
  serialiseRequest _ r =
    let b = requestBody r
        lbs = W.urlEncodeAsForm b
    in r { requestBody = DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs) }

instance (W.FromForm body) => ResponsePayload body UrlFormPayload where
  deserialiseRequest _ resp = do
    let stream = responseBody resp
    bs <- S.fold SEB.write $ stream
    body <- either (throw . DecodingException) return . W.urlDecodeAsForm $ LB.fromStrict bs
    return $ resp { responseBody = body }

-- | A type tag used to indicate that a request\/response should be encoded\/decoded as @application/x-www-form-urlencoded@ data
urlForm :: QProxy UrlFormPayload
urlForm = QProxy

data EmptyPayload = EmptyPayload

instance HasMediaType EmptyPayload where
  mediaType _ = Nothing

instance RequestPayload Empty EmptyPayload where
  serialiseRequest _ r = r { requestBody = DefinedContentLength 0 S.nil }

instance ResponsePayload Empty EmptyPayload where
  deserialiseRequest _ resp = do
    let stream = responseBody resp
    body <- Empty <$ S.drain stream
    return $ resp { responseBody = body }

-- | A type tag used to indicate that a request\/response has no payload
noPayload :: QProxy EmptyPayload
noPayload = QProxy

decodeTextContent :: (MonadThrow m, MonadIO m) => HttpResponse (SerialT m Word8) -> m (HttpResponse T.Text)
decodeTextContent resp = do
  let contentTypeHV = getHeaderValue "Content-Type" resp
  mediaType' <- traverse MTH.parseMediaType contentTypeHV
  let maybeCharset = mediaType' >>= Map.lookup "charset" . MTH.parameters
  let stream = responseBody resp
  bs <- S.fold SEB.write $ stream
  return $ resp { responseBody = decodeContent maybeCharset bs }
    where
      decodeContent maybeCharset bs' = 
        case fmap CI.mk maybeCharset of
          Just("utf8")       -> TE.decodeUtf8 bs'
          Just("iso-8859-1") -> TE.decodeLatin1 bs'
          _                  -> TE.decodeUtf8 bs'

data HtmlPayload = HtmlPayload

instance HasMediaType HtmlPayload where
  mediaType _ = Just textHtml

instance RequestPayload T.Text HtmlPayload where
  serialiseRequest _ r =
    let b = requestBody r
        lbs = LB.fromStrict $ TE.encodeUtf8 b 
    in r { requestBody = DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs) }

instance ResponsePayload T.Text HtmlPayload where
  deserialiseRequest _ resp = decodeTextContent resp

-- | A type tag used to indicate that a request\/response should be encoded\/decoded as @text/html@ data
html :: QProxy HtmlPayload
html = QProxy

-- TO THINK ABOUT: Can we have a type tag to encode OR possibilities?  e.g. json or text for cases where the server may return application/json for successful responses and text/plain for errors

