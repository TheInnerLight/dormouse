
module Dormouse.Backend
  ( RequestBackend(..)
  , ResponseBackend(..)
  ) where
  
import qualified Network.HTTP.Client as C
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

class RequestBackend t where
  writeResponseBody :: t -> C.RequestBody

instance RequestBackend LB.ByteString where
  writeResponseBody = C.RequestBodyLBS

class ResponseBackend t where
  readResponseBody :: (C.Response C.BodyReader -> IO (C.Response t))

instance ResponseBackend LB.ByteString where
  readResponseBody res = do
    bss <- C.brConsume $ C.responseBody res
    return res { C.responseBody = LB.fromChunks bss }

instance ResponseBackend SB.ByteString where
  readResponseBody res = do
    bss <- C.brConsume $ C.responseBody res
    return res { C.responseBody = LB.toStrict $ LB.fromChunks bss }


