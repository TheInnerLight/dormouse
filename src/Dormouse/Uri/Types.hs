{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Dormouse.Uri.Types
  ( UriReference(..)
  , Authority(..)
  , Fragment(..)
  , Host(..)
  , Path(..)
  , PathSegment(..)
  , Query(..)
  , Scheme(..)
  , Username(..)
  , Password(..)
  , UserInfo(..)
  , Uri(..)
  ) where

import Control.Exception.Safe (MonadThrow, throw, Exception(..))
import Data.Proxy
import qualified Data.List as L
import Data.Text (Text, unpack)
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift(..))

newtype Username = Username { unUsername :: Text } deriving (Eq, Lift)

instance Show Username where
  show username = unpack $ unUsername username

newtype Password = Password { unPassword :: Text } deriving (Eq, Lift)

instance Show Password where
  show _ = "****"

data UserInfo = UserInfo 
  { userInfoUsername :: Username
  , userInfoPassword :: Maybe Password
  } deriving (Eq, Show, Lift)

newtype Host = Host { unHost :: Text } deriving (Eq, Lift)

instance Show Host where
  show host = unpack $ unHost host

data Authority = Authority 
  { authorityUserInfo ::  Maybe UserInfo
  , authorityHost :: Host
  , authorityPort :: Maybe Int
  } deriving (Eq, Show, Lift)

newtype Fragment = Fragment { unFragment :: Text } deriving (Eq, Lift)

instance Show Fragment where
  show fragment = unpack $ unFragment fragment

data UriReference = Absolute | Relative | Unknown

newtype Path (ref :: UriReference) = Path { unPath :: [PathSegment]} deriving (Eq, Lift)

instance Show (Path ref) where
  show path = "[" <> L.intercalate "," (fmap show . unPath $ path) <> "]"

newtype PathSegment = PathSegment { unPathSegment :: Text } deriving (Eq, Lift)

instance Show PathSegment where
  show seg = unpack $ unPathSegment seg

newtype Query = Query { unQuery :: Text } deriving (Eq, Lift)

instance Show Query where
  show query = unpack $ unQuery query

newtype Scheme = Scheme { unScheme :: Text } deriving (Eq, Lift)

instance Show Scheme where
  show scheme = unpack . unScheme $ scheme


data Uri (ref :: UriReference) (scheme :: Symbol) where 
  AbsoluteUri :: Scheme -> Maybe Authority -> Path Absolute -> Maybe Query -> Maybe Fragment -> Uri Absolute scheme
  RelativeUri :: Path Relative -> Maybe Query -> Maybe Fragment -> Uri Relative scheme
  AbsOrRelUri :: Uri r s -> Uri Unknown scheme

instance Lift (Uri ref scheme) where
  lift (AbsoluteUri scheme auth path queryParams fragment) = [| AbsoluteUri scheme auth path queryParams fragment |]
  lift (RelativeUri path queryParams fragment) = [| RelativeUri path queryParams fragment |]
  lift (AbsOrRelUri absUri) = [| AbsOrRelUri |]

instance Show (Uri ref scheme) where
  show (AbsoluteUri scheme auth path queryParams fragment) = "AbsoluteUri { scheme = " <> show scheme <> ", authority = " <> show auth <> ", path = " <> show path <> ", queryParams = " <> show queryParams <> ", fragment = " <> show fragment <> " }"
  show (RelativeUri path queryParams fragment) = "RelativeUri { path = " <> show path <> ", queryParams = " <> show queryParams <> ", fragment = " <> show fragment <> " }"
  show (AbsOrRelUri absUri) = show absUri

instance Eq (Uri ref scheme) where
  (==) (AbsoluteUri scheme1 auth1 path1 queryParams1 fragment1) (AbsoluteUri scheme2 auth2 path2 queryParams2 fragment2) = scheme1 == scheme2 && auth1 == auth2 && path1 == path2 && queryParams1 == queryParams2 && fragment1 == fragment2
  (==) (RelativeUri path1 queryParams1 fragment1) (RelativeUri path2 queryParams2 fragment2) = path1 == path2 && queryParams1 == queryParams2 && fragment1 == fragment2
  (==) (AbsOrRelUri (AbsoluteUri scheme1 auth1 path1 queryParams1 fragment1)) (AbsOrRelUri (AbsoluteUri scheme2 auth2 path2 queryParams2 fragment2)) = scheme1 == scheme2 && auth1 == auth2 && path1 == path2 && queryParams1 == queryParams2 && fragment1 == fragment2
  (==) (AbsOrRelUri (RelativeUri path1 queryParams1 fragment1)) (AbsOrRelUri (RelativeUri path2 queryParams2 fragment2)) = path1 == path2 && queryParams1 == queryParams2 && fragment1 == fragment2
  (==) _ _ = False
