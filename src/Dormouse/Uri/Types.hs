{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , AbsUri(..)
  , RelUri(..)
  ) where

import Control.Exception.Safe (MonadThrow, throw, Exception(..))
import Data.Proxy
import Data.String (IsString(..))
import qualified Data.List as L
import Data.Text (Text, unpack, pack)
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift(..))

newtype Username = Username { unUsername :: Text } deriving (Eq, Lift)

instance Show Username where
  show username = unpack $ unUsername username

instance IsString Username where
  fromString s = Username $ pack s

newtype Password = Password { unPassword :: Text } deriving (Eq, Lift)

instance Show Password where
  show _ = "****"

instance IsString Password where
  fromString s = Password $ pack s

data UserInfo = UserInfo 
  { userInfoUsername :: Username
  , userInfoPassword :: Maybe Password
  } deriving (Eq, Show, Lift)

newtype Host = Host { unHost :: Text } deriving (Eq, Lift)

instance IsString Host where
  fromString s = Host $ pack s

instance Show Host where
  show host = unpack $ unHost host

data Authority = Authority 
  { authorityUserInfo ::  Maybe UserInfo
  , authorityHost :: Host
  , authorityPort :: Maybe Int
  } deriving (Eq, Show, Lift)

newtype Fragment = Fragment { unFragment :: Text } deriving (Eq, Lift)

instance IsString Fragment where
  fromString s = Fragment $ pack s

instance Show Fragment where
  show fragment = unpack $ unFragment fragment

data UriReference = Absolute | Relative | Unknown

newtype Path (ref :: UriReference) = Path { unPath :: [PathSegment]} deriving (Eq, Lift)

instance Show (Path ref) where
  show path = "[" <> L.intercalate "," (fmap show . unPath $ path) <> "]"

newtype PathSegment = PathSegment { unPathSegment :: Text } deriving (Eq, Lift)

instance IsString PathSegment where
  fromString s = PathSegment $ pack s

instance Show PathSegment where
  show seg = unpack $ unPathSegment seg

newtype Query = Query { unQuery :: Text } deriving (Eq, Lift)

instance IsString Query where
  fromString s = Query $ pack s

instance Show Query where
  show query = unpack $ unQuery query

newtype Scheme = Scheme { unScheme :: Text } deriving (Eq, Lift)

instance Show Scheme where
  show scheme = unpack . unScheme $ scheme

data AbsUri = AbsUri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath :: Path Absolute
  , uriQuery :: Maybe Query
  , uriFragment :: Maybe Fragment
  } deriving (Eq, Show, Lift)

data RelUri = RelUri
  { uriPath :: Path Relative
  , uriQuery :: Maybe Query
  , uriFragment :: Maybe Fragment
  } deriving (Eq, Show, Lift)

data Uri (ref :: UriReference) (scheme :: Symbol) where 
  AbsoluteUri :: AbsUri -> Uri Absolute scheme
  RelativeUri :: RelUri -> Uri Relative scheme
  AbsOrRelUri :: Uri r s -> Uri Unknown scheme

instance Lift (Uri ref scheme) where
  lift (AbsoluteUri u)      = [| AbsoluteUri u |]
  lift (RelativeUri u)      = [| RelativeUri u |]
  lift (AbsOrRelUri absUri) = [| AbsOrRelUri |]

instance Show (Uri ref scheme) where
  show (AbsoluteUri u) = show u
  show (RelativeUri u) = show u
  show (AbsOrRelUri absUri) = show absUri

instance Eq (Uri ref scheme) where
  (==) (AbsoluteUri u1) (AbsoluteUri u2) = u1 == u2
  (==) (RelativeUri u1) (RelativeUri u2) = u1 == u2
  (==) (AbsOrRelUri (AbsoluteUri u1)) (AbsOrRelUri (AbsoluteUri u2)) = u1 == u2
  (==) (AbsOrRelUri (RelativeUri u1)) (AbsOrRelUri (RelativeUri u2)) = u1 == u2
  (==) _ _ = False
