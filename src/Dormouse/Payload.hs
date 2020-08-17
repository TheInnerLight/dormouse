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
  , DecodingException(..)
  , JsonLbsPayload
  , UrlFormPayload
  , RequestPayload(..)
  , json
  , urlForm
  , noBody
  ) where

import Control.Exception.Safe (Exception, MonadThrow(..), throw)
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, Value, encode, fromEncoding, eitherDecode, eitherDecodeStrict)
import Data.Functor.Const
import Data.Proxy
import Data.Text (Text, pack)
import Data.Word (Word8, Word64)
import GHC.Exts
import qualified Data.ByteString  as SB
import qualified Data.ByteString.Lazy as LB
import qualified Web.FormUrlEncoded as W
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.External.ByteString.Lazy as SEBL
import Streamly.Internal.Memory.Array.Types (Array(..))

class HasAcceptHeader tag where
  acceptHeader :: Proxy tag -> Maybe SB.ByteString

class HasContentType tag where
  contentType :: Proxy tag -> Maybe SB.ByteString

data RequestPayload
  -- | DefinedContentLength represents a payload where the size of the message is known in advance and the content length header can be computed
  = DefinedContentLength Word64 (SerialT IO (Array Word8))
  -- | ChunkedTransfer represents a payload with indertiminate length, to be sent using chunked transfer encoding
  | ChunkedTransfer (SerialT IO (Array Word8))  

-- | HttpPayload relates a payload tag to low level request and response representations and the constraints required to encode and decode to/from that type
class (HasContentType tag, HasAcceptHeader tag) => HttpPayload tag where
  -- | 'RequestPayloadConstraint' describes the constraints on `b` such that it can be encoded as the low level representation
  type RequestPayloadConstraint tag b :: Constraint
  -- | `ResponsePayloadConstraint' describes the constraints on `b` such that it can be decoded from the low level representation.
  type ResponsePayloadConstraint tag b :: Constraint
  -- | Generates a low level payload representation from the supplied content
  createRequestPayload :: RequestPayloadConstraint tag b => Proxy tag -> b -> RequestPayload
  -- | Generates high level content from the supplied low level payload representation
  extractResponsePayload :: (ResponsePayloadConstraint tag b, MonadIO m, MonadThrow m) => Proxy tag -> SerialT IO (Array Word8) -> m b

data JsonLbsPayload = JsonPayload

instance HasAcceptHeader JsonLbsPayload where
  acceptHeader _ = Just "application/json"

instance HasContentType JsonLbsPayload where
  contentType _ = Just "application/json"

instance HttpPayload JsonLbsPayload where
  type RequestPayloadConstraint JsonLbsPayload b = ToJSON b
  type ResponsePayloadConstraint JsonLbsPayload b = FromJSON b
  createRequestPayload _ b = 
    DefinedContentLength (fromIntegral . LB.length $ lbs) (SEBL.toChunks lbs)
      where
       lbs = encode b
  extractResponsePayload _ stream = do
    lbs <- liftIO $ SEBL.fromChunksIO stream
    either (throw . DecodingException . pack) return . eitherDecode $ lbs

json :: Proxy JsonLbsPayload
json = Proxy :: Proxy JsonLbsPayload

data UrlFormPayload = UrlFormPayload

instance HasAcceptHeader UrlFormPayload where
  acceptHeader _ = Just "application/x-www-form-urlencoded"

instance HasContentType UrlFormPayload where
  contentType _ = Just "application/x-www-form-urlencoded"

instance HttpPayload UrlFormPayload where
  type RequestPayloadConstraint UrlFormPayload b = W.ToForm b
  type ResponsePayloadConstraint UrlFormPayload b = W.FromForm b
  createRequestPayload _ b = 
    DefinedContentLength (fromIntegral . LB.length $ lbs) (SEBL.toChunks lbs)
    where
      lbs = W.urlEncodeAsForm b
  extractResponsePayload _ stream = do
    lbs <- liftIO $ SEBL.fromChunksIO stream
    either (throw . DecodingException) return $ W.urlDecodeAsForm lbs

urlForm :: Proxy UrlFormPayload
urlForm = Proxy :: Proxy UrlFormPayload

data DecodingException = DecodingException Text
  deriving (Show)

instance Exception DecodingException

data EmptyPayload = EmptyPayload

instance HasAcceptHeader EmptyPayload where
  acceptHeader _ = Nothing

instance HasContentType EmptyPayload where
  contentType _ = Nothing

instance HttpPayload EmptyPayload where
  type RequestPayloadConstraint EmptyPayload b = b ~ ()
  type ResponsePayloadConstraint EmptyPayload b = b ~ ()
  createRequestPayload _ b = DefinedContentLength 0 S.nil
  extractResponsePayload _ stream = liftIO $ S.drain stream

noBody :: Proxy EmptyPayload
noBody = Proxy :: Proxy EmptyPayload
