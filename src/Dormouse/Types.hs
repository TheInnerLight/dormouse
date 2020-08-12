
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Dormouse.Types
  ( HttpRequest(..)
  , HttpResponse(..)
  , SomeDormouseException(..)
  , UnexpectedStatusCode(..)
  ) where

import Control.Exception.Safe (Exception(..))
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Dormouse.Payload
import Dormouse.Headers
import Dormouse.Methods
import Dormouse.Uri.Types 
import qualified Data.ByteString as SB

data HttpRequest scheme method body contentTag acceptTag = HttpRequest 
  { requestMethod :: HttpMethod method
  , requestUri :: Uri Absolute scheme
  , requestHeaders :: [(HeaderName, SB.ByteString)]
  , requestBody :: body
  } deriving (Eq, Show)

data HttpResponse b = HttpResponse
  { responseStatusCode :: Int
  , responseHeaders :: [(HeaderName, SB.ByteString)]
  , responseBody :: b
  } deriving (Show, Eq)

data SomeDormouseException = forall e . Exception e => SomeDormouseException e

instance Show SomeDormouseException where
    show (SomeDormouseException e) = show e

instance Exception SomeDormouseException

data UnexpectedStatusCode = UnexpectedStatusCode { uscStatusCode :: Int }

instance Show (UnexpectedStatusCode) where
  show (UnexpectedStatusCode { uscStatusCode = statusCode }) = "Server returned unexpected status code: " <> show statusCode

instance Exception (UnexpectedStatusCode) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a
