{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Dormouse.Uri.Query 
  ( QueryComponent
  , QueryBuilder
  , (&)
  , (?)
  , (=:)
  ) where

import qualified Data.Text as T
import Data.Foldable  
import Data.Sequence (Seq(..))
import qualified Data.Sequence as SQ
import Dormouse.Uri.Types

data QueryComponent
  = QueryParam T.Text T.Text
  | QueryFlag T.Text
  deriving (Eq, Show)

newtype QueryBuilder = QueryBuilder { unQueryBuilder :: SQ.Seq QueryComponent }
  deriving (Eq, Show)

instance Semigroup QueryBuilder where
  x1 <> x2 = QueryBuilder $ (unQueryBuilder x1) <> (unQueryBuilder x2)

instance Monoid QueryBuilder where
  mempty = QueryBuilder SQ.empty

class IsQueryVal a where
  toQueryVal :: a -> T.Text

instance IsQueryVal Bool where toQueryVal = T.pack . show
instance IsQueryVal Char where toQueryVal = T.pack . show
instance IsQueryVal Double where toQueryVal = T.pack . show
instance IsQueryVal Float where toQueryVal = T.pack . show
instance IsQueryVal Int where toQueryVal = T.pack . show
instance IsQueryVal Integer where toQueryVal = T.pack . show
instance IsQueryVal (String) where toQueryVal = T.pack
instance IsQueryVal (T.Text) where toQueryVal = id

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

(=:) :: IsQueryVal a => T.Text -> a -> QueryBuilder
(=:) t1 t2 = QueryBuilder . SQ.singleton $ QueryParam t1 (toQueryVal t2)
