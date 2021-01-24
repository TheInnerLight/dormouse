{-# LANGUAGE GADTs #-}

module Dormouse.Url.Class 
  ( IsUrl(..)
  ) where

import Dormouse.Url.Types

class (Eq url, Show url) => IsUrl url where
  asAnyUrl :: url -> AnyUrl

instance IsUrl (Url scheme) where
  asAnyUrl = AnyUrl

instance IsUrl AnyUrl where
  asAnyUrl (AnyUrl u) = asAnyUrl u
