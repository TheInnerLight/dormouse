{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveLift #-}

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

import Data.String (IsString(..))
import qualified Data.List as L
import Data.Text (Text, unpack, pack)
import Language.Haskell.TH.Syntax (Lift(..))

-- | The Username subcomponent of a URI UserInfo
newtype Username = Username { unUsername :: Text } deriving (Eq, Lift)

instance Show Username where
  show username = unpack $ unUsername username

instance IsString Username where
  fromString s = Username $ pack s

-- | The Password subcomponent of a URI UserInfo
newtype Password = Password { unPassword :: Text } deriving (Eq, Lift)

instance Show Password where
  show _ = "****"

instance IsString Password where
  fromString s = Password $ pack s

-- | The UserInfo subcomponent of a URI Authority
data UserInfo = UserInfo 
  { userInfoUsername :: Username
  , userInfoPassword :: Maybe Password
  } deriving (Eq, Show, Lift)

-- | The Host subcomponent of a URI Authority
newtype Host = Host { unHost :: Text } deriving (Eq, Lift)

instance IsString Host where
  fromString s = Host $ pack s

instance Show Host where
  show host = unpack $ unHost host

-- | The Authority component of a URI
data Authority = Authority 
  { authorityUserInfo :: Maybe UserInfo
  , authorityHost :: Host
  , authorityPort :: Maybe Int
  } deriving (Eq, Show, Lift)

-- | The Fragment component of a URI
newtype Fragment = Fragment { unFragment :: Text } deriving (Eq, Lift)

instance IsString Fragment where
  fromString s = Fragment $ pack s

instance Show Fragment where
  show fragment = unpack $ unFragment fragment

data UriReference = Absolute | Relative

-- | The Path component of a URI, including a series of individual Path Segments
newtype Path (ref :: UriReference) = Path { unPath :: [PathSegment]} deriving (Eq, Lift)

instance Show (Path ref) where
  show path = "[" <> L.intercalate "," (fmap show . unPath $ path) <> "]"

-- | An individial Path segment of a URI
newtype PathSegment = PathSegment { unPathSegment :: Text } deriving (Eq, Lift)

instance IsString PathSegment where
  fromString s = PathSegment $ pack s

instance Show PathSegment where
  show seg = unpack $ unPathSegment seg

-- | The Query component of a URI
newtype Query = Query { unQuery :: Text } deriving (Eq, Lift)

instance IsString Query where
  fromString s = Query $ pack s

instance Show Query where
  show query = unpack $ unQuery query

-- | The Scheme component of a URI
newtype Scheme = Scheme { unScheme :: Text } deriving (Eq, Lift)

instance Show Scheme where
  show scheme = unpack . unScheme $ scheme

-- | The data associated with an Absolute URI
data AbsUri = AbsUri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath :: Path 'Absolute
  , uriQuery :: Maybe Query
  , uriFragment :: Maybe Fragment
  } deriving (Eq, Show, Lift)

-- | The data associated with a Relative URI
data RelUri = RelUri
  { uriPath :: Path 'Relative
  , uriQuery :: Maybe Query
  , uriFragment :: Maybe Fragment
  } deriving (Eq, Show, Lift)

-- | A Uniform Resource Identifier (URI) is a compact sequence of characters that identifies an abstract or physical resource.
-- It is defined according to RFC 3986 (<https://tools.ietf.org/html/rfc3986>).  URIs can be absolute (i.e. defined against a
-- specific scheme) or relative.
data Uri 
  = AbsoluteUri AbsUri
  | RelativeUri RelUri
  deriving (Lift, Eq, Show)


