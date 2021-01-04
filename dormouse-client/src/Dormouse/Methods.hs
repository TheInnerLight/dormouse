{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.Methods
  ( HttpMethod(..)
  , AllowedBody
  , methodAsByteString
  ) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as C8SB
import Data.Kind (Constraint)
import Data.Proxy
import Dormouse.Data
import GHC.TypeLits

data HttpMethod (a :: Symbol) where 
  CONNECT :: HttpMethod "CONNECT"
  DELETE :: HttpMethod "DELETE"
  HEAD :: HttpMethod "HEAD"
  GET :: HttpMethod "GET"
  OPTIONS :: HttpMethod "OPTIONS"
  PATCH :: HttpMethod "PATCH"
  POST :: HttpMethod "POST"
  PUT :: HttpMethod "PUT"
  TRACE :: HttpMethod "TRACE"
  CUSTOM :: KnownSymbol a => Proxy a -> HttpMethod a

instance Show (HttpMethod a) where
  show CONNECT    = "CONNECT"
  show DELETE     = "DELETE"
  show HEAD       = "HEAD"
  show GET        = "GET"
  show OPTIONS    = "OPTIONS"
  show PATCH      = "PATCH"
  show POST       = "POST"
  show PUT        = "PUT"
  show TRACE      = "TRACE"
  show (CUSTOM p)   = show . symbolVal $ p

instance Eq (HttpMethod a) where
  (==) _ _ = True

type family AllowedBody (a :: Symbol) b :: Constraint
type instance AllowedBody "CONNECT" b = (b ~ Empty)
type instance AllowedBody "DELETE" b = ()
type instance AllowedBody "GET" b = (b ~ Empty)
type instance AllowedBody "HEAD" b = (b ~ Empty)
type instance AllowedBody "OPTIONS" b = (b ~ Empty)
type instance AllowedBody "PATCH" b = ()
type instance AllowedBody "POST" b = ()
type instance AllowedBody "PUT" b = ()
type instance AllowedBody "TRACE" b = (b ~ Empty)

methodAsByteString :: HttpMethod a -> SB.ByteString
methodAsByteString CONNECT    = "CONNECT"
methodAsByteString DELETE     = "DELETE"
methodAsByteString HEAD       = "HEAD"
methodAsByteString GET        = "GET"
methodAsByteString OPTIONS    = "OPTIONS"
methodAsByteString PATCH      = "PATCH"
methodAsByteString POST       = "POST"
methodAsByteString PUT        = "PUT"
methodAsByteString TRACE      = "TRACE"
methodAsByteString (CUSTOM p) = C8SB.pack . symbolVal $ p
