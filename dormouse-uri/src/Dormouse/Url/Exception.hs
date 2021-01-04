{-# LANGUAGE ExistentialQuantification #-}

module Dormouse.Url.Exception
  ( UrlException(..)
  ) where

import Control.Exception.Safe (Exception(..))
import qualified Data.Text as T

-- | 'UrlException' is used to indicate an error in transforming a valid URI into a URL, e.g. the URI refers to a different schema such as @file@
data UrlException = UrlException  { urlExceptionMessage :: T.Text }

instance Show (UrlException) where
  show (UrlException { urlExceptionMessage = msg }) = "Failed to parse url: " <> T.unpack msg

instance Exception (UrlException)
