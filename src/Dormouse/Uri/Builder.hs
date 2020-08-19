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

(</>) :: Uri ref scheme -> Text -> Uri ref scheme
(</>) (AbsoluteUri AbsUri {uriPath = path, .. }) text = AbsoluteUri $ AbsUri {uriPath = (Path {unPath =  (unPath path) ++ [PathSegment text] }), ..}
(</>) (RelativeUri RelUri {uriPath = path, .. }) text = RelativeUri $ RelUri {uriPath = (Path {unPath =  (unPath path) ++ [PathSegment text] }), ..}

(&) :: QueryBuilder -> QueryBuilder -> QueryBuilder
(&) = (<>)

(?) :: Uri a b -> QueryBuilder -> Uri a b
(?) uri b = 
  case uri of 
    AbsoluteUri (AbsUri { uriQuery = q, .. }) -> AbsoluteUri $ AbsUri { uriQuery = Just $ foldl' folder "" $ unQueryBuilder b  , .. }
    RelativeUri (RelUri { uriQuery = q, .. }) -> RelativeUri $ RelUri { uriQuery = Just $ foldl' folder "" $ unQueryBuilder b  , .. }
    AbsOrRelUri underlying                    -> AbsOrRelUri $ underlying ? b
  where 
    folder "" (QueryParam key val)  = Query $ key <> "=" <> val
    folder acc (QueryParam key val) = Query $ unQuery acc <> "&" <> key <> "=" <> val

infixl 8 ?

(=:) :: IsQueryVal a => Text -> a -> QueryBuilder
(=:) t1 t2 = QueryBuilder . SQ.singleton $ QueryParam t1 (toQueryVal t2)
