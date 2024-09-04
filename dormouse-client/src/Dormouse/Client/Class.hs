module Dormouse.Client.Class
  ( MonadDormouseClient(..)
  , HasDormouseClientConfig(..)
  , DormouseClientConfig(..)
  ) where

import Data.Word ( Word8 )
import Dormouse.Client.Payload ( RawRequestPayload(..) )
import Dormouse.Client.Types ( HttpRequest(..), HttpResponse(..) )
import Dormouse.Url ( IsUrl )
import Network.HTTP.Client ( Manager )
import qualified Streamly.Data.Stream as Stream

-- | The configuration options required to run Dormouse
newtype DormouseClientConfig = DormouseClientConfig { clientManager :: Manager }

-- | Describes the capability to retrieve a Dormouse Config
class HasDormouseClientConfig a where
  getDormouseClientConfig :: a -> DormouseClientConfig

instance HasDormouseClientConfig DormouseClientConfig where
  getDormouseClientConfig = id

-- | MonadDormouseClient describes the capability to send HTTP requests and receive an HTTP response
class Monad m => MonadDormouseClient m where
  -- | Sends a supplied HTTP request and retrieves a response within the supplied monad @m@
  send :: IsUrl url => HttpRequest url method RawRequestPayload contentTag acceptTag -> (HttpResponse (Stream.Stream IO Word8) -> IO (HttpResponse b)) -> m (HttpResponse b)
