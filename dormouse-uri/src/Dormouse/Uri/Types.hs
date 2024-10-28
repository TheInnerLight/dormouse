{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveLift #-}

module Dormouse.Uri.Types
  ( UriReferenceType(..)
  , Authority(..)
  , Fragment(..)
  , Host(..)
  , Path(..)
  , PathSegment(..)
  , Query(..)
  , Scheme(..)
  , UserInfo(..)
  , Uri(..)
  , RelRef(..)
  , UriReference(..)
  ) where

import Data.String (IsString(..))
import qualified Data.List as L
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift(..))

-- | The UserInfo subcomponent of a URI Authority
newtype UserInfo = UserInfo 
  { unUserInfo :: Text
  } deriving (Eq, Ord, Lift)

instance Show UserInfo where
  show userInfo = -- applications should not render as clear text anything after the first colon
    case T.split (==':') $ unUserInfo userInfo of 
      [] -> ""
      [x] -> unpack x
      x:_ -> unpack x <> ":****"

-- | The Host subcomponent of a URI Authority
newtype Host = Host { unHost :: Text } deriving (Eq, Ord, Lift)

instance IsString Host where
  fromString s = Host $ pack s

instance Show Host where
  show host = unpack $ unHost host

-- | The Authority component of a URI
data Authority = Authority 
  { authorityUserInfo :: Maybe UserInfo
  , authorityHost :: Host
  , authorityPort :: Maybe Int
  } deriving (Eq, Ord, Show, Lift)

-- | The Fragment component of a URI
newtype Fragment = Fragment { unFragment :: Text } deriving (Eq, Ord, Lift)

instance IsString Fragment where
  fromString s = Fragment $ pack s

instance Show Fragment where
  show fragment = unpack $ unFragment fragment

data UriReferenceType = Absolute | Relative

-- | The Path component of a URI, including a series of individual Path Segments
newtype Path (ref :: UriReferenceType) = Path { unPath :: [PathSegment]} deriving (Eq, Ord, Lift)

instance Show (Path ref) where
  show path = "[" <> L.intercalate "," (fmap show . unPath $ path) <> "]"

-- | An individial Path segment of a URI
newtype PathSegment = PathSegment { unPathSegment :: Text } deriving (Eq, Ord, Lift)

instance IsString PathSegment where
  fromString s = PathSegment $ pack s

instance Show PathSegment where
  show seg = unpack $ unPathSegment seg

-- | The Query component of a URI
newtype Query = Query { unQuery :: Text } deriving (Eq, Ord, Lift)

instance IsString Query where
  fromString s = Query $ pack s

instance Show Query where
  show query = unpack $ unQuery query

-- | The Scheme component of a URI
newtype Scheme = Scheme { unScheme :: Text } deriving (Eq, Ord, Lift)

instance Show Scheme where
  show scheme = unpack . unScheme $ scheme

-- | A Uniform Resource Identifier (URI) is a compact sequence of characters that identifies an abstract or physical resource.
-- It is defined according to RFC 3986 (<https://tools.ietf.org/html/rfc3986>).
data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath :: Path 'Absolute
  , uriQuery :: Maybe Query
  , uriFragment :: Maybe Fragment
  } deriving (Eq, Ord, Show, Lift)

-- | The data associated with a URI Relative Reference
data RelRef = RelRef
  { relRefAuthority :: Maybe Authority
  , relRefPath :: Path 'Relative
  , relRefQuery :: Maybe Query
  , relRefFragment :: Maybe Fragment
  } deriving (Eq, Ord, Show, Lift)

-- | A URI-reference is either a URI or a relative reference.  If the URI-reference's prefix does not match the syntax of a scheme 
-- followed by its colon separator, then the URI-reference is a relative reference.
data UriReference 
  = AbsoluteUri Uri
  | RelativeRef RelRef
  deriving (Lift, Ord, Eq, Show)


