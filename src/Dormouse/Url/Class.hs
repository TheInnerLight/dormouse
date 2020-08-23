{-# LANGUAGE GADTs #-}

module Dormouse.Url.Class 
  ( IsUrl(..)
  ) where

import Dormouse.Url.Types

class (Eq url, Show url) => IsUrl url where
  createRequest :: url -> (UrlScheme, UrlComponents)

instance IsUrl (Url scheme) where
  createRequest (HttpUrl uc)  = (HttpScheme, uc)
  createRequest (HttpsUrl uc) = (HttpsScheme, uc)

instance IsUrl (AnyUrl) where
  createRequest (AnyUrl u) = createRequest u
