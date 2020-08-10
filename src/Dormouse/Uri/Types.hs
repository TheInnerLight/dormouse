{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Uri.Types
  ( UriReference(..)
  , Authority(..)
  , Fragment(..)
  , Host(..)
  , Path(..)
  , PathSegment(..)
  , QueryParam(..)
  , Scheme(..)
  , Username(..)
  , Password(..)
  , UserInfo(..)
  , Uri(..)
  ) where

import Data.Text (Text)
import GHC.TypeLits

newtype Username = Username { unUsername :: Text }
newtype Password = Password { unPassword :: Text }

data UserInfo = UserInfo 
  { userInfoUsername :: Username
  , userInfoPassword :: Maybe Password
  }

newtype Host = Host { unHost :: Text }

data Authority = Authority
  { authorityUserInfo ::  Maybe UserInfo
  , authorityHost :: Host
  , authorityPort :: Maybe Int
  }

newtype Fragment = Fragment { unFragment :: Text }

data UriReference = Absolute | Relative | Unknown

newtype Path = Path { unPath :: [PathSegment]}

newtype PathSegment = PathSegment { unPathSegment :: Text }

data QueryParam 
  = QueryFlag Text
  | QueryParam Text Text

newtype Scheme = Scheme { unScheme :: Text }

data Uri (ref :: UriReference) (scheme :: Symbol) where
  AbsoluteUri :: Scheme -> Authority -> Uri Relative scheme' -> Uri Absolute scheme
  RelativeUri :: Path -> [QueryParam] -> Fragment -> Uri Relative scheme
  UnknownAbs :: Uri Absolute scheme -> Uri Unknown scheme
  UnknownRel :: Uri Relative scheme -> Uri Unknown scheme
