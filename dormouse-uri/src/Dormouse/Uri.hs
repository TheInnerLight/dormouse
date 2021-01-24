{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Uri
  ( module Dormouse.Uri.Types
  , parseUri
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.ByteString as SB
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.Text as T
import Dormouse.Uri.Exception (UriException(..))
import Dormouse.Uri.Parser
import Dormouse.Uri.Types

-- | Parse an ascii 'ByteString' as an absolute uri, throwing a 'UriException' in @m@ if this fails
parseUri :: MonadThrow m => SB.ByteString -> m Uri
parseUri bs = either (throw . UriException . T.pack) return $ parseOnly pUri bs
