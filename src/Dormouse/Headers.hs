
module Dormouse.Headers
  ( HeaderName
  , HasHeaders(..)
  ) where

import qualified Data.ByteString  as SB
import Data.CaseInsensitive  (CI)
import qualified Data.Map.Strict as Map
import Data.Foldable

type HeaderName = CI SB.ByteString

class HasHeaders a where
  getHeaders :: a -> Map.Map HeaderName SB.ByteString
  getHeaderValue :: HeaderName -> a -> Maybe SB.ByteString

