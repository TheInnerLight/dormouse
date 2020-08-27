{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Dormouse.Uri.Query 
  ( QueryComponent(..)
  , QueryBuilder(..)
  , IsQueryVal(..)
  ) where

import qualified Data.Text as T
import qualified Data.Sequence as SQ

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
