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
  , OctetStreamPayload
  , UrlFormPayload
  , json
  , octetStream
  , urlForm
  , noBody
  ) where

import Control.Exception.Safe (Exception, MonadThrow(..), throw)
import Data.Aeson (FromJSON, ToJSON, Value, encode, fromEncoding, eitherDecode, eitherDecodeStrict)
import Data.Functor.Const
import Data.Proxy
import Data.Text (Text, pack)
import Dormouse.Backend
import GHC.Exts
import qualified Data.ByteString  as SB
import qualified Data.ByteString.Lazy as LB
import qualified Web.FormUrlEncoded as W

class HasAcceptHeader tag where
  acceptHeader :: Proxy tag -> Maybe SB.ByteString

class HasContentType tag where
  contentType :: Proxy tag -> Maybe SB.ByteString

-- | HttpPayload relates a payload tag to low level request and response representations and the constraints required to encode and decode to/from that type
class (RequestBackend (RawReqPayload(tag)), ResponseBackend (RawRespPayload(tag)), HasContentType tag, HasAcceptHeader tag) => HttpPayload tag where
  -- | 'RawReqPayload' is the low level representation used for HTTP requests.
  type RawReqPayload tag :: *
  -- | 'RawRespPayload' is the low level representation used for HTTP responses.
  type RawRespPayload tag :: *
  -- | 'RequestPayloadConstraint' describes the constraints on `b` such that it can be encoded as the low level representation
  type RequestPayloadConstraint tag b :: Constraint
  -- | `ResponsePayloadConstraint' describes the constraints on `b` such that it can be decoded from the low level representation.
  type ResponsePayloadConstraint tag b :: Constraint
  -- | Generates a low level payload representation from the supplied content
  createRequestPayload :: RequestPayloadConstraint tag b => Proxy tag -> b -> RawReqPayload tag
  -- | Generates high level content from the supplied low level payload representation
  extractResponsePayload :: (ResponsePayloadConstraint tag b, MonadThrow m) => Proxy tag -> RawRespPayload tag -> m b

data JsonLbsPayload = JsonPayload

instance HasAcceptHeader JsonLbsPayload where
  acceptHeader _ = Just "application/json"

instance HasContentType JsonLbsPayload where
  contentType _ = Just "application/json"

instance HttpPayload JsonLbsPayload where
  type RequestPayloadConstraint JsonLbsPayload b = ToJSON b
  type ResponsePayloadConstraint JsonLbsPayload b = FromJSON b
  type RawReqPayload JsonLbsPayload = LB.ByteString
  type RawRespPayload JsonLbsPayload = LB.ByteString
  createRequestPayload _ b = encode b
  extractResponsePayload _ lbs = either (throw . DecodingException . pack) return $ eitherDecode lbs

json :: Proxy JsonLbsPayload
json = Proxy :: Proxy JsonLbsPayload

data OctetStreamPayload = OctetStreamPayload

instance HasAcceptHeader OctetStreamPayload where
  acceptHeader _ = Just "application/octet-stream"

instance HasContentType OctetStreamPayload where
  contentType _ = Just "application/octet-stream"

instance HttpPayload OctetStreamPayload where
  type RequestPayloadConstraint OctetStreamPayload b = b ~ LB.ByteString
  type ResponsePayloadConstraint OctetStreamPayload b = b ~ LB.ByteString
  type RawReqPayload OctetStreamPayload = LB.ByteString
  type RawRespPayload OctetStreamPayload = LB.ByteString
  createRequestPayload _ b = b
  extractResponsePayload _ b = return b

octetStream :: Proxy OctetStreamPayload
octetStream = Proxy :: Proxy OctetStreamPayload

data UrlFormPayload = UrlFormPayload

instance HasAcceptHeader UrlFormPayload where
  acceptHeader _ = Just "application/x-www-form-urlencoded"

instance HasContentType UrlFormPayload where
  contentType _ = Just "application/x-www-form-urlencoded"

instance HttpPayload UrlFormPayload where
  type RequestPayloadConstraint UrlFormPayload b = W.ToForm b
  type ResponsePayloadConstraint UrlFormPayload b = W.FromForm b
  type RawReqPayload UrlFormPayload = LB.ByteString
  type RawRespPayload UrlFormPayload = LB.ByteString
  createRequestPayload _ b = W.urlEncodeAsForm b
  extractResponsePayload _ lbs = either (throw . DecodingException) return $ W.urlDecodeAsForm lbs

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
  type RawReqPayload EmptyPayload = LB.ByteString
  type RawRespPayload EmptyPayload = LB.ByteString
  createRequestPayload _ b = LB.empty
  extractResponsePayload _ _ = return ()

noBody :: Proxy EmptyPayload
noBody = Proxy :: Proxy EmptyPayload
