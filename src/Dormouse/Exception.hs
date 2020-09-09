{-# LANGUAGE ExistentialQuantification #-}

module Dormouse.Exception 
  ( SomeDormouseException(..)
  , DecodingException(..)
  , UnexpectedStatusCode(..)
  , UriException(..)
  , UrlException(..)
  , MediaTypeException(..)
  ) where

  
import Control.Exception.Safe (Exception(..))
import qualified Data.Text as T
import Data.Typeable (cast)

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

data DecodingException = DecodingException { decodingExceptionMessage :: T.Text }

instance Show (DecodingException) where
  show (DecodingException { decodingExceptionMessage = msg }) = "Server returned unexpected status code: " <> T.unpack msg

instance Exception DecodingException

data UriException = UriException  { uriExceptionMessage :: T.Text }

instance Show (UriException) where
  show (UriException { uriExceptionMessage = msg }) = "Failed to parse uri: " <> T.unpack msg

instance Exception (UriException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

data UrlException = UrlException  { urlExceptionMessage :: T.Text }

instance Show (UrlException) where
  show (UrlException { urlExceptionMessage = msg }) = "Failed to parse uri: " <> T.unpack msg

instance Exception (UrlException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

data MediaTypeException = MediaTypeException  { mediaTypeExceptionMessage :: T.Text }

instance Show (MediaTypeException) where
  show (MediaTypeException { mediaTypeExceptionMessage = msg }) = "Failed to parse media type: " <> show msg

instance Exception (MediaTypeException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a


