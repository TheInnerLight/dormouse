{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Uri
  ( module Dormouse.Uri.Types
  , parseUri
  , QueryBuilder
  , IsQueryVal(..)
  , IsUrl(..)
  ) where

import Control.Exception.Safe (MonadThrow, throw, Exception(..))
import qualified Data.ByteString as SB
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Text (Text, pack)
import Data.Typeable (cast)
import Dormouse.Types (SomeDormouseException(..))
import Dormouse.Uri.Parser
import Dormouse.Uri.Types
import Dormouse.Uri.Query
import Dormouse.Url.Class

data UriException = UriException  { uriExceptionMessage :: Text }

instance Show (UriException) where
  show (UriException { uriExceptionMessage = message }) = "Failed to parse uri: " <> show message

instance Exception (UriException) where
  toException    = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

-- | Parse an ascii 'ByteString' as an absolute uri, throwing a 'UriException' in @m@ if this fails
parseUri :: MonadThrow m => SB.ByteString -> m Uri
parseUri bs = either (throw . UriException . pack) (return) $ parseOnly pUri bs
