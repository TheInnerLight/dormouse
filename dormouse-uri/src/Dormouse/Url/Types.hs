{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Dormouse.Url.Types
  ( UrlComponents(..)
  , UrlScheme(..)
  , Url(..)
  , AnyUrl(..)
  ) where

import Dormouse.Uri.Types
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift(..))

data UrlComponents = UrlComponents
  { urlAuthority :: Authority
  , urlPath :: Path 'Absolute
  , urlQuery :: Maybe Query
  , urlFragment :: Maybe Fragment
  } deriving (Eq, Show, Lift)

data UrlScheme
  = HttpScheme
  | HttpsScheme

-- | A 'Url' is defined here as an absolute URI in the @http@ or @https@ schemes.  Authority components are requried by the http / https
-- Uri schemes.
data Url (scheme :: Symbol) where
  HttpUrl  :: UrlComponents -> Url "http"
  HttpsUrl :: UrlComponents -> Url "https"

instance Eq (Url scheme) where
  (==) (HttpUrl u1)  (HttpUrl u2)  = show u1 == show u2
  (==) (HttpsUrl u1) (HttpsUrl u2) = show u1 == show u2

instance Show (Url scheme) where
  show (HttpUrl wu)  = "http " <> show wu
  show (HttpsUrl wu) = "https " <> show wu

instance Lift (Url scheme) where
  lift (HttpUrl uc)  = [| HttpUrl uc |]
  lift (HttpsUrl uc) = [| HttpsUrl uc |]

-- | `AnyUrl` is a wrapper aroud `Url` which allows either @http@ or @https@ urls.
data AnyUrl = forall scheme. AnyUrl (Url scheme)

instance Eq AnyUrl where
  (==) (AnyUrl (HttpUrl d1)) (AnyUrl (HttpUrl d2))   = d1 == d2
  (==) (AnyUrl (HttpsUrl d1)) (AnyUrl (HttpsUrl d2)) = d1 == d2
  (==) _  _                                          = False

instance Show AnyUrl where
  show (AnyUrl u) = show u

instance Lift (AnyUrl) where
  lift (AnyUrl u)  = [| AnyUrl u |]
