module Dormouse.Uri
  ( parseRequestFromUri
  ) where

import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import qualified Data.ByteString as SB
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import Dormouse.Types (SomeDormouseException(..), UriException(..))
import qualified Network.HTTP.Client as C
import URI.ByteString (URI, URIRef(..), Absolute, Host(..), Scheme(..), Port(..), Authority(..), Query(..))

serialiseQuery :: [(SB.ByteString, SB.ByteString)] -> SB.ByteString
serialiseQuery [] = SB.empty
serialiseQuery xs = "?" <> (SB.intercalate "&" $ fmap (\(h,v) -> h <> "=" <> v) $ xs)

parseRequestFromUri :: MonadThrow m => URI -> m C.Request
parseRequestFromUri uri = do
  authority <- maybe (throw UriException {uriExceptionUri = uri, uriExceptionMessage = "Uri had no valid authority"}) return (uriAuthority uri)
  let host = hostBS . authorityHost $ authority
  let isSecure = (schemeBS . uriScheme $ uri) == "https"
  let port = maybe (if isSecure then 443 else 80) (portNumber) (authorityPort authority)
  let queryParams = queryPairs . uriQuery $ uri 
  return $ C.defaultRequest { C.host = host, C.path = uriPath uri, C.secure = isSecure, C.port = port, C.queryString = serialiseQuery queryParams }
