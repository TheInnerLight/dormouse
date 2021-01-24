{-# LANGUAGE ExistentialQuantification #-}

module Dormouse.Uri.Exception
  ( UriException(..)
  ) where

import Control.Exception.Safe (Exception(..))
import qualified Data.Text as T

-- | 'UriException' is used to indicate an error parsing a URI
newtype UriException = UriException  { uriExceptionMessage :: T.Text }

instance Show UriException where
  show UriException { uriExceptionMessage = msg } = "Failed to parse uri: " <> T.unpack msg

instance Exception UriException
