module Dormouse.Class
  ( MonadDormouse(..)
  , HasDormouseConfig(..)
  , DormouseConfig(..)
  ) where

import Data.Word ( Word8 )
import Dormouse.Payload ( RequestPayload(..) )
import Dormouse.Types ( HttpRequest(..), HttpResponse(..) )
import Network.HTTP.Client ( Manager )
import Streamly ( SerialT )

-- | The configuration options required to run Dormouse
data DormouseConfig = DormouseConfig { clientManager :: Manager }

-- | Describes the capability to retrieve a Dormouse Config
class HasDormouseConfig a where
  getDormouseConfig :: a -> DormouseConfig

instance HasDormouseConfig DormouseConfig where
  getDormouseConfig = id

-- -- | MonadDormouse describes the capability to send HTTP requests and receive an HTTP response
class Monad m => MonadDormouse m where
  -- | Sends a supplied HTTP request and retrieves a response within the supplied monad @m@
  send :: HttpRequest url method RequestPayload contentTag acceptTag -> (HttpResponse (SerialT IO Word8) -> IO (HttpResponse b)) -> m (HttpResponse b)
