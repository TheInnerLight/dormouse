{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}

module Dormouse.Payload
  ( HasMediaType(..)
  , EmptyPayload
  , RequestPayload(..)
  , ResponsePayload(..)
  , JsonPayload
  , UrlFormPayload
  , HtmlPayload
  , RawRequestPayload(..)
  , json
  , urlForm
  , noPayload
  , html
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import qualified Data.CaseInsensitive as CI
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import Dormouse.Data
import Dormouse.Types
import Dormouse.Exception (DecodingException(..))
import Dormouse.Headers
import Dormouse.Headers.MediaType
import qualified Dormouse.Headers.MediaType as MTH
import GHC.Exts
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Web.FormUrlEncoded as W
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString as SEB
import qualified Streamly.External.ByteString.Lazy as SEBL

-- | Describes an association between a type @tag@ and a specific Media Type
class HasMediaType tag where
  mediaType :: Proxy tag -> Maybe MediaType

-- | A raw HTTP Request payload consisting of a stream of bytes with either a defined Content Length or using Chunked Transfer Encoding
data RawRequestPayload
  -- | DefinedContentLength represents a payload where the size of the message is known in advance and the content length header can be computed
  = DefinedContentLength Word64 (SerialT IO Word8)
  -- | ChunkedTransfer represents a payload with indertiminate length, to be sent using chunked transfer encoding
  | ChunkedTransfer (SerialT IO Word8)  

-- | RequestPayload relates a type of content to its byte stream representation and the constraints required to encode it
class HasMediaType tag => RequestPayload tag where
  -- | @RequestPayloadConstraint@ describes the constraints on @b@ such that it can be encoded as the low level representation
  type RequestPayloadConstraint tag b :: Constraint
  -- | Generates a the byte stream representation from the supplied content
  serialiseRequest :: RequestPayloadConstraint tag b => Proxy tag -> HttpRequest url method b acceptTag contentTag -> HttpRequest url method RawRequestPayload acceptTag contentTag

-- | ResponsePayload relates a type of content to its byte stream representation and the constraints required to decode it
class HasMediaType tag => ResponsePayload tag where
  -- | @ResponsePayloadConstraint@ describes the constraints on @b@ such that it can be decoded from the low level representation.
  type ResponsePayloadConstraint tag b :: Constraint
  -- | Decodes the high level representation from the supplied byte stream
  deserialiseRequest :: (ResponsePayloadConstraint tag b) => Proxy tag -> HttpResponse (SerialT IO Word8) -> IO (HttpResponse b)

data JsonPayload = JsonPayload

instance HasMediaType JsonPayload where
  mediaType _ = Just applicationJson

instance RequestPayload JsonPayload where
  type RequestPayloadConstraint JsonPayload b = ToJSON b
  serialiseRequest _ r = 
    let b = requestBody r
        lbs = encode b
    in r { requestBody = DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs) }

instance ResponsePayload JsonPayload where
  type ResponsePayloadConstraint JsonPayload b = FromJSON b
  deserialiseRequest _ resp = do
    let stream = responseBody resp
    bs <- S.fold SEB.write $ stream
    body <- either (throw . DecodingException . T.pack) return . eitherDecodeStrict $ bs
    return $ resp { responseBody = body }

json :: Proxy JsonPayload
json = Proxy :: Proxy JsonPayload

data UrlFormPayload = UrlFormPayload

instance HasMediaType UrlFormPayload where
  mediaType _ = Just applicationXWWWFormUrlEncoded

instance RequestPayload UrlFormPayload where
  type RequestPayloadConstraint UrlFormPayload b = W.ToForm b
  serialiseRequest _ r =
    let b = requestBody r
        lbs = W.urlEncodeAsForm b
    in r { requestBody = DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs) }

instance ResponsePayload UrlFormPayload where
  type ResponsePayloadConstraint UrlFormPayload b = W.FromForm b
  deserialiseRequest _ resp = do
    let stream = responseBody resp
    bs <- S.fold SEB.write $ stream
    body <- either (throw . DecodingException) return . W.urlDecodeAsForm $ LB.fromStrict bs
    return $ resp { responseBody = body }

urlForm :: Proxy UrlFormPayload
urlForm = Proxy :: Proxy UrlFormPayload

data EmptyPayload = EmptyPayload

instance HasMediaType EmptyPayload where
  mediaType _ = Nothing

instance RequestPayload EmptyPayload where
  type RequestPayloadConstraint EmptyPayload b = b ~ Empty
  serialiseRequest _ r = r { requestBody = DefinedContentLength 0 S.nil }

instance ResponsePayload EmptyPayload where
  type ResponsePayloadConstraint EmptyPayload b = b ~ Empty
  deserialiseRequest _ resp = do
    let stream = responseBody resp
    body <- fmap (const Empty) $ S.drain stream
    return $ resp { responseBody = body }

noPayload :: Proxy EmptyPayload
noPayload = Proxy :: Proxy EmptyPayload

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

instance RequestPayload HtmlPayload where
  type RequestPayloadConstraint HtmlPayload b = b ~ T.Text
  serialiseRequest _ r =
    let b = requestBody r
        lbs = LB.fromStrict $ TE.encodeUtf8 b 
    in r { requestBody = DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs) }

instance ResponsePayload HtmlPayload where
  type ResponsePayloadConstraint HtmlPayload b = b ~ T.Text
  deserialiseRequest _ resp = decodeTextContent resp

html :: Proxy HtmlPayload
html = Proxy :: Proxy HtmlPayload
