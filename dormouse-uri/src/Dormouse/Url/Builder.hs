{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
  
-- | Provides syntax for incrementally building Urls and components of Urls such as Query strings
module Dormouse.Url.Builder 
  ( QueryComponent(..)
  , QueryBuilder(..)
  , IsQueryVal(..)
  ,  (</>)
  , (?)
  , (&)
  , (=:)
  ) where

import Data.Foldable  
import Data.Text (Text)
import Dormouse.Uri.Types
import Dormouse.Url.Types

import qualified Data.Text as T
import qualified Data.Sequence as SQ

-- | An individual Query Parameter or Query Flag
data QueryComponent
  = QueryParam T.Text T.Text
  | QueryFlag T.Text
  deriving (Eq, Show)

-- | Stores a series of Query Parameters or Query Flags used for building the Query component of a URI/URL, for example in the format @?param1=2&param2=3&flag1@
newtype QueryBuilder = QueryBuilder { unQueryBuilder :: SQ.Seq QueryComponent }
  deriving (Eq, Show)

instance Semigroup QueryBuilder where
  x1 <> x2 = QueryBuilder $ unQueryBuilder x1 <> unQueryBuilder x2

instance Monoid QueryBuilder where
  mempty = QueryBuilder SQ.empty

-- | Signifies a type that can be rendered as Query Parameter within a URI
class IsQueryVal a where
  toQueryVal :: a -> T.Text

instance IsQueryVal Bool where toQueryVal = T.pack . show
instance IsQueryVal Char where toQueryVal = T.pack . show
instance IsQueryVal Double where toQueryVal = T.pack . show
instance IsQueryVal Float where toQueryVal = T.pack . show
instance IsQueryVal Int where toQueryVal = T.pack . show
instance IsQueryVal Integer where toQueryVal = T.pack . show
instance IsQueryVal String where toQueryVal = T.pack
instance IsQueryVal T.Text where toQueryVal = id

-- | Combine a Url with a new text path component
(</>) :: Url scheme -> Text -> Url scheme
(</>) (HttpUrl UrlComponents {urlPath = path, .. }) text = HttpUrl $ UrlComponents {urlPath = (Path {unPath = unPath path ++ [PathSegment text] }), ..}
(</>) (HttpsUrl UrlComponents {urlPath = path, .. }) text = HttpsUrl $ UrlComponents {urlPath = (Path {unPath = unPath path ++ [PathSegment text] }), ..}

-- | Convenient alias for '<>' which allows for combining query parameters
(&) :: QueryBuilder -> QueryBuilder -> QueryBuilder
(&) = (<>)

-- | Combine a Url with a some supplied query parameters
(?) :: Url scheme -> QueryBuilder -> Url scheme
(?) uri b = 
  case uri of 
    HttpUrl  UrlComponents { .. } -> HttpUrl $ UrlComponents { urlQuery = Just $ foldl' folder "" $ unQueryBuilder b  , .. }
    HttpsUrl UrlComponents { .. } -> HttpsUrl $ UrlComponents { urlQuery = Just $ foldl' folder "" $ unQueryBuilder b  , .. }
  where 
    folder "" (QueryFlag val)       = Query $ val
    folder "" (QueryParam key val)  = Query $ key <> "=" <> val
    folder acc (QueryFlag val)      = Query $ unQuery acc <> "&" <> val
    folder acc (QueryParam key val) = Query $ unQuery acc <> "&" <> key <> "=" <> val

infixl 8 ?

-- | Generate a query paramter of the form @key=value@
(=:) :: IsQueryVal a => Text -> a -> QueryBuilder
(=:) key value = QueryBuilder . SQ.singleton $ QueryParam key (toQueryVal value)
