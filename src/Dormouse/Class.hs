{-# LANGUAGE TypeFamilies #-}

module Dormouse.Class
  ( MonadDormouse(..)
  ) where

import Data.Kind (Constraint)
import Dormouse.Types

class MonadDormouse m where 
  type MonadHttpConstraint m tag acceptTag :: Constraint
  send :: MonadHttpConstraint m tag acceptTag => HttpRequest method tag acceptTag -> m (HttpResponse acceptTag)
