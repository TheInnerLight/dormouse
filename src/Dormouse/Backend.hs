
module Dormouse.Backend
  ( RequestBackend(..)
  , ResponseBackend(..)
  ) where
  
import qualified Network.HTTP.Client as C
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB

class RequestBackend t where
  writeRequestBody :: t -> C.RequestBody

instance RequestBackend LB.ByteString where
  writeRequestBody = C.RequestBodyLBS

class ResponseBackend t where
  readResponseBody :: (C.BodyReader -> IO t)

instance ResponseBackend LB.ByteString where
  readResponseBody res = do
    bss <- C.brConsume res
    return $ LB.fromChunks bss
