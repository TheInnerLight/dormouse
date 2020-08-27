
module Dormouse.Headers
  ( HeaderName
  , HasHeaders(..)
  ) where

import qualified Data.ByteString  as SB
import Data.CaseInsensitive  (CI)
import qualified Data.Map.Strict as Map

-- | The name of an HTTP Header.  Header names are case insensitive.
type HeaderName = CI SB.ByteString

-- | Describes something which has headers
class HasHeaders a where
  -- | Retrieve all of the headers which @a@ has.
  getHeaders :: a -> Map.Map HeaderName SB.ByteString
  -- | Try to retrieve a specific header from @a@ with the supplied `HeaderName`
  getHeaderValue :: HeaderName -> a -> Maybe SB.ByteString

