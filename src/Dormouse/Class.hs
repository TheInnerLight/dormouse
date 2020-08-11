{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.Class
  ( MonadDormouse(..)
  , HasDormouseConfig(..)
  , DormouseConfig(..)
  ) where

import Data.Kind (Constraint)
import Dormouse.Backend
import Dormouse.Payload
import Dormouse.Types
import Network.HTTP.Client (Manager)

data DormouseConfig = DormouseConfig { clientManager :: Manager }

class HasDormouseConfig a where
  getDormouseConfig :: a -> DormouseConfig

instance HasDormouseConfig DormouseConfig where
  getDormouseConfig = id

-- | MonadDormouse describes the capability to send HTTP requests and receive an HTTP response
class Monad m => MonadDormouse m where
  -- | Sends a supplied HTTP request and retrieves a response within the supplied monad 'm'
  send :: (HttpPayload tag, HttpPayload acceptTag) => HttpRequest scheme method tag acceptTag -> m (HttpResponse acceptTag)
