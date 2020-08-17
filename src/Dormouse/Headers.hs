
module Dormouse.Headers
  ( HeaderName
  , getHeaderValue
  ) where

import qualified Data.ByteString  as SB
import Data.CaseInsensitive  (CI)
import Data.Foldable

type HeaderName = CI SB.ByteString

getHeaderValue :: Foldable t => HeaderName -> t (HeaderName, SB.ByteString) -> Maybe SB.ByteString
getHeaderValue h = fmap snd . find (\(h', _) -> h == h')
