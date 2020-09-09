{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Dormouse.Class
  ( MonadDormouse(..)
  , HasDormouseConfig(..)
  , DormouseConfig(..)
  ) where

import qualified Data.ByteString  as SB
import Data.Word ( Word8 )
import Dormouse.Headers ( HeaderName )
import Dormouse.Payload ( HttpPayload, RequestPayload )
import Dormouse.Types ( HttpRequest, HttpResponse )
import Network.HTTP.Client ( Manager )
import Streamly ( SerialT )
import qualified Data.Map.Strict as Map

-- | The configuration options required to run Dormouse
data DormouseConfig = DormouseConfig { clientManager :: Manager }

-- | Describes the capability to retrieve a Dormouse Config
class HasDormouseConfig a where
  getDormouseConfig :: a -> DormouseConfig

instance HasDormouseConfig DormouseConfig where
  getDormouseConfig = id

-- | MonadDormouse describes the capability to send HTTP requests and receive an HTTP response
class Monad m => MonadDormouse m where
  -- | Sends a supplied HTTP request and retrieves a response within the supplied monad @m@
  send :: (HttpPayload contentTag, HttpPayload acceptTag) => HttpRequest scheme method a contentTag acceptTag -> (a -> RequestPayload) -> (Map.Map HeaderName SB.ByteString -> SerialT IO Word8 -> IO b) -> m (HttpResponse b)
