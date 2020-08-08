{-# LANGUAGE TypeFamilies #-}

module Dormouse.Class
  ( MonadDormouse(..)
  , HasDormouseConfig(..)
  , DormouseConfig(..)
  ) where

import Data.Kind (Constraint)
import Dormouse.Types
import Network.HTTP.Client (Manager)

data DormouseConfig = DormouseConfig { clientManager :: Manager }

class HasDormouseConfig a where
  getDormouseConfig :: a -> DormouseConfig

instance HasDormouseConfig DormouseConfig where
  getDormouseConfig = id

class MonadDormouse m where 
  type MonadHttpConstraint m tag acceptTag :: Constraint
  send :: MonadHttpConstraint m tag acceptTag => HttpRequest method tag acceptTag -> m (HttpResponse acceptTag)
