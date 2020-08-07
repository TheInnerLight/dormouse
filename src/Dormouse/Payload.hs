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
  , JsonSbsPayload
  , json
  , jsonStrict
  ) where

import Control.Exception.Safe (Exception, MonadThrow(..), throw)
import Data.Aeson (FromJSON, ToJSON, Value, encode, fromEncoding, eitherDecode, eitherDecodeStrict)
import Data.Functor.Const
import Data.Proxy
import GHC.Exts
import qualified Data.ByteString  as SB
import qualified Data.ByteString.Lazy as LB

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
  extractResponsePayload _ lbs = either (\s -> throw $ DecodingException s) (\x -> return  x) $ eitherDecode lbs

json :: Proxy JsonLbsPayload
json = Proxy :: Proxy JsonLbsPayload

data JsonSbsPayload = JsonSbsPayload

instance HasAcceptHeader JsonSbsPayload where
  acceptHeader _ = Just "application/json"

instance HasContentType JsonSbsPayload where
  contentType _ = Just "application/json"

instance HttpPayload JsonSbsPayload where
  type RequestPayloadConstraint JsonSbsPayload b = ToJSON b
  type ResponsePayloadConstraint JsonSbsPayload b = FromJSON b
  type RawPayload JsonSbsPayload = SB.ByteString
  createRequestPayload _ b = LB.toStrict $ encode b
  extractResponsePayload _ sbs = either (\s -> throw $ DecodingException s) (\x -> return  x) $ eitherDecodeStrict sbs

jsonStrict :: Proxy JsonSbsPayload
jsonStrict = Proxy :: Proxy JsonSbsPayload

data DecodingException = DecodingException String
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
