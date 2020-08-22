{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
  
module Dormouse.Uri.Builder 
  ( (</>)
  ,  (?)
  , (&)
  , (=:)
  ) where

import Data.Foldable  
import qualified Data.Sequence as SQ
import Data.Text (Text, unpack, pack)
import Dormouse.Uri.Query
import Dormouse.Uri.Types

-- | Combine a Url with a new text path component
(</>) :: Url scheme -> Text -> Url scheme
(</>) (HttpUrl UrlComponents {urlPath = path, .. }) text = HttpUrl $ UrlComponents {urlPath = (Path {unPath =  (unPath path) ++ [PathSegment text] }), ..}
(</>) (HttpsUrl UrlComponents {urlPath = path, .. }) text = HttpsUrl $ UrlComponents {urlPath = (Path {unPath =  (unPath path) ++ [PathSegment text] }), ..}

-- | Convenient alias for '<>' which allows for combining query parameters
(&) :: QueryBuilder -> QueryBuilder -> QueryBuilder
(&) = (<>)

-- | Combine a Url with a some supplied query parameters
(?) :: Url scheme -> QueryBuilder -> Url scheme
(?) uri b = 
  case uri of 
    HttpUrl (UrlComponents { urlQuery = q, .. }) -> HttpUrl $ UrlComponents { urlQuery = Just $ foldl' folder "" $ unQueryBuilder b  , .. }
    HttpsUrl (UrlComponents { urlQuery = q, .. }) -> HttpsUrl $ UrlComponents { urlQuery = Just $ foldl' folder "" $ unQueryBuilder b  , .. }
  where 
    folder "" (QueryParam key val)  = Query $ key <> "=" <> val
    folder acc (QueryParam key val) = Query $ unQuery acc <> "&" <> key <> "=" <> val

infixl 8 ?

-- | Generate a query paramter of the form @key=value@
(=:) :: IsQueryVal a => Text -> a -> QueryBuilder
(=:) key value = QueryBuilder . SQ.singleton $ QueryParam key (toQueryVal value)
