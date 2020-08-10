{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Dormouse.Payload
  ( LBSRawPayload(..)
  , HasAcceptHeader(..)
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
import GHC.Exts
import qualified Data.ByteString  as SB
import qualified Data.ByteString.Lazy as LB
import qualified Web.FormUrlEncoded as W

newtype LBSRawPayload = LBSRawPayload {unLBSRawPayload :: LB.ByteString} deriving (Eq, Show)

class HasAcceptHeader tag where
  acceptHeader :: Proxy tag -> Maybe SB.ByteString

class HasContentType tag where
  contentType :: Proxy tag -> Maybe SB.ByteString

class (HasContentType tag, HasAcceptHeader tag) => HttpPayload tag where
  type RequestPayloadConstraint tag b :: Constraint
  type ResponsePayloadConstraint tag b :: Constraint
  type RawPayload tag :: *
  createRequestPayload :: RequestPayloadConstraint tag b => Proxy tag -> b -> RawPayload tag
  extractResponsePayload :: (ResponsePayloadConstraint tag b, MonadThrow m) => Proxy tag -> RawPayload tag -> m b

data JsonLbsPayload = JsonPayload

instance HasAcceptHeader JsonLbsPayload where
  acceptHeader _ = Just "application/json"

instance HasContentType JsonLbsPayload where
  contentType _ = Just "application/json"

instance HttpPayload JsonLbsPayload where
  type RequestPayloadConstraint JsonLbsPayload b = ToJSON b
  type ResponsePayloadConstraint JsonLbsPayload b = FromJSON b
  type RawPayload JsonLbsPayload = LB.ByteString
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
  type RawPayload OctetStreamPayload = LB.ByteString
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
  type RawPayload UrlFormPayload = LB.ByteString
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
  type RawPayload EmptyPayload = LB.ByteString
  createRequestPayload _ b = LB.empty
  extractResponsePayload _ _ = return ()

noBody :: Proxy EmptyPayload
noBody = Proxy :: Proxy EmptyPayload
