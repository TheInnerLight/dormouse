
module Dormouse.Headers
  ( HeaderName
  ) where

import qualified Data.ByteString  as SB
import Data.CaseInsensitive  (CI)

type HeaderName = CI SB.ByteString
