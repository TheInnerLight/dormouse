{-# LANGUAGE ExistentialQuantification #-}

module Dormouse.Exception 
  ( SomeDormouseException(..)
  , DecodingException(..)
  , UnexpectedStatusCodeException(..)
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

-- | 'UnexpectedStatusCodeException' is used to indicate that the remote server returned an unexpected status code value, for instance an unsuccessful (non-2XX) status code.
data UnexpectedStatusCodeException = UnexpectedStatusCodeException { uscStatusCode :: Int }

instance Show (UnexpectedStatusCodeException) where
  show (UnexpectedStatusCodeException { uscStatusCode = statusCode }) = "Server returned unexpected status code: " <> show statusCode

instance Exception (UnexpectedStatusCodeException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

-- | 'DecodingException' is used to when something has gone wrong decoding an http response into the expected representation, e.g. json was expected but the response json was invalid.
data DecodingException = DecodingException { decodingExceptionMessage :: T.Text }

instance Show (DecodingException) where
  show (DecodingException { decodingExceptionMessage = msg }) = "Decoding payload failed: " <> T.unpack msg

instance Exception DecodingException

-- | 'UriException' is used to indicate an error parsing a URI
data UriException = UriException  { uriExceptionMessage :: T.Text }

instance Show (UriException) where
  show (UriException { uriExceptionMessage = msg }) = "Failed to parse uri: " <> T.unpack msg

instance Exception (UriException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

-- | 'UrlException' is used to indicate an error in transforming a valid URI into a URL, e.g. the URI refers to a different schema such as @file@
data UrlException = UrlException  { urlExceptionMessage :: T.Text }

instance Show (UrlException) where
  show (UrlException { urlExceptionMessage = msg }) = "Failed to parse url: " <> T.unpack msg

instance Exception (UrlException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

-- | 'MediaTypeException' is used to indicate an error parsing a MediaType header such as "Content-Type" into a valid 'MediaType'
data MediaTypeException = MediaTypeException  { mediaTypeExceptionMessage :: T.Text }

instance Show (MediaTypeException) where
  show (MediaTypeException { mediaTypeExceptionMessage = msg }) = "Failed to parse media type: " <> show msg

instance Exception (MediaTypeException) where
  toException     = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a


