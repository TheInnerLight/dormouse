{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}

module Dormouse.Payload
  ( HasAcceptHeader(..)
  , HasContentType(..)
  , EmptyPayload(..)
  , HttpPayload(..)
  , JsonPayload
  , UrlFormPayload
  , RequestPayload(..)
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
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import Dormouse.Data
import Dormouse.Exception (DecodingException(..))
import Dormouse.Headers
import qualified Dormouse.Headers.MediaType as MTH
import GHC.Exts
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Web.FormUrlEncoded as W
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString as SEB
import qualified Streamly.External.ByteString.Lazy as SEBL

class HasAcceptHeader tag where
  acceptHeader :: Proxy tag -> Maybe SB.ByteString

class HasContentType tag where
  contentType :: Proxy tag -> Maybe SB.ByteString

data RequestPayload
  -- | DefinedContentLength represents a payload where the size of the message is known in advance and the content length header can be computed
  = DefinedContentLength Word64 (SerialT IO Word8)
  -- | ChunkedTransfer represents a payload with indertiminate length, to be sent using chunked transfer encoding
  | ChunkedTransfer (SerialT IO Word8)  

-- | HttpPayload relates a payload tag to low level request and response representations and the constraints required to encode and decode to/from that type
class (HasContentType tag, HasAcceptHeader tag) => HttpPayload tag where
  -- | 'RequestPayloadConstraint' describes the constraints on `b` such that it can be encoded as the low level representation
  type RequestPayloadConstraint tag b :: Constraint
  -- | `ResponsePayloadConstraint' describes the constraints on `b` such that it can be decoded from the low level representation.
  type ResponsePayloadConstraint tag b :: Constraint
  -- | Generates a low level payload representation from the supplied content
  createRequestPayload :: RequestPayloadConstraint tag b => Proxy tag -> b -> RequestPayload
  -- | Generates high level content from the supplied low level payload representation
  extractResponsePayload :: (ResponsePayloadConstraint tag b, MonadIO m, MonadThrow m) => Proxy tag -> Map.Map HeaderName SB.ByteString -> SerialT IO Word8  -> m b

data JsonPayload = JsonPayload

instance HasAcceptHeader JsonPayload where
  acceptHeader _ = Just "application/json"

instance HasContentType JsonPayload where
  contentType _ = Just "application/json"

instance HttpPayload JsonPayload where
  type RequestPayloadConstraint JsonPayload b = ToJSON b
  type ResponsePayloadConstraint JsonPayload b = FromJSON b
  createRequestPayload _ b = 
    let lbs = encode b
    in DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs)
  extractResponsePayload _ _ stream = do
    bs <- liftIO $ S.fold SEB.write $ stream
    either (throw . DecodingException . pack) return . eitherDecodeStrict $ bs

json :: Proxy JsonPayload
json = Proxy :: Proxy JsonPayload

data UrlFormPayload = UrlFormPayload

instance HasAcceptHeader UrlFormPayload where
  acceptHeader _ = Just "application/x-www-form-urlencoded"

instance HasContentType UrlFormPayload where
  contentType _ = Just "application/x-www-form-urlencoded"

instance HttpPayload UrlFormPayload where
  type RequestPayloadConstraint UrlFormPayload b = W.ToForm b
  type ResponsePayloadConstraint UrlFormPayload b = W.FromForm b
  createRequestPayload _ b = 
    let lbs = W.urlEncodeAsForm b
    in DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs)
  extractResponsePayload _ _ stream = do
    bs <- liftIO $ S.fold SEB.write $ stream
    either (throw . DecodingException) return . W.urlDecodeAsForm $ LB.fromStrict bs

urlForm :: Proxy UrlFormPayload
urlForm = Proxy :: Proxy UrlFormPayload

data EmptyPayload = EmptyPayload

instance HasAcceptHeader EmptyPayload where
  acceptHeader _ = Nothing

instance HasContentType EmptyPayload where
  contentType _ = Nothing

instance HttpPayload EmptyPayload where
  type RequestPayloadConstraint EmptyPayload b = b ~ Empty
  type ResponsePayloadConstraint EmptyPayload b = b ~ Empty
  createRequestPayload _ _ = DefinedContentLength 0 S.nil
  extractResponsePayload _ _ stream = fmap (const Empty) $ liftIO $ S.drain stream

noPayload :: Proxy EmptyPayload
noPayload = Proxy :: Proxy EmptyPayload

decodeTextContent :: (MonadThrow m) => Map.Map (CI.CI SB.ByteString) SB.ByteString -> SB.ByteString -> m Text
decodeTextContent headers bs = do
  let contentTypeHV = Map.lookup "Content-Type" headers
  mediaType <- traverse MTH.parseMediaType contentTypeHV
  let maybeCharset = mediaType >>= Map.lookup "charset" . MTH.parameters
  return $ decodeContent maybeCharset bs
    where
      decodeContent maybeCharset bs' = 
        case fmap CI.mk maybeCharset of
          Just("utf8")       -> TE.decodeUtf8 bs'
          Just("iso-8859-1") -> TE.decodeLatin1 bs'
          _                  -> TE.decodeUtf8 bs'

data HtmlPayload = HtmlPayload

instance HasAcceptHeader HtmlPayload where
  acceptHeader _ = Just "text/html"

instance HasContentType HtmlPayload where
  contentType _ = Just "text/html"

instance HttpPayload HtmlPayload where
  type RequestPayloadConstraint HtmlPayload b = b ~ Text
  type ResponsePayloadConstraint HtmlPayload b = b ~ Text
  createRequestPayload _ b = 
    let lbs = LB.fromStrict $ TE.encodeUtf8 b 
    in DefinedContentLength (fromIntegral . LB.length $ lbs) (S.unfold SEBL.read lbs)
  extractResponsePayload _ headers stream = do
    bs <- liftIO $ S.fold SEB.write $ stream
    decodeTextContent headers bs

html :: Proxy HtmlPayload
html = Proxy :: Proxy HtmlPayload
