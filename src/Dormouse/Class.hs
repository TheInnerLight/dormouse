{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.Class
  ( MonadDormouse(..)
  , HasDormouseConfig(..)
  , DormouseConfig(..)
  ) where

import Data.Kind (Constraint)
import Data.Word (Word8)
import Dormouse.Payload
import Dormouse.Types
import Network.HTTP.Client (Manager)
import Streamly
import Streamly.Memory.Array (Array)

data DormouseConfig = DormouseConfig { clientManager :: Manager }

class HasDormouseConfig a where
  getDormouseConfig :: a -> DormouseConfig

instance HasDormouseConfig DormouseConfig where
  getDormouseConfig = id

-- | MonadDormouse describes the capability to send HTTP requests and receive an HTTP response
class Monad m => MonadDormouse m where
  -- | Sends a supplied HTTP request and retrieves a response within the supplied monad 'm'
  send :: (HttpPayload contentTag, HttpPayload acceptTag) => HttpRequest scheme method a contentTag acceptTag -> (a -> RequestPayload) -> (SerialT IO (Array Word8) -> IO b) -> m (HttpResponse b)
