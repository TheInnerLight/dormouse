{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Uri
  ( parseRequestFromUri
  , (//)
  , (?:)
  , (=:)
  ) where

import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NL
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable, cast)
import Dormouse.Types (SomeDormouseException(..), UriException(..))
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Types.URI as UT
import Text.URI (URI(..), Authority (..), QueryParam(..), unRText, RText, RTextLabel(..))

unQueryParam :: QueryParam -> (Text, Maybe Text)
unQueryParam (QueryFlag flag) = (unRText flag, Nothing)
unQueryParam (QueryParam k v) = (unRText k, Just $ unRText v)

(//) :: URI -> RText PathPiece -> URI
(//) uri pathPiece = 
  case uriPath uri of
    Nothing                 -> uri {uriPath = Just (False, [pathPiece])}
    Just (trailing, pieces) -> uri {uriPath = Just(trailing, NL.cons pathPiece pieces)}

(?:) :: URI -> [QueryParam] -> URI
(?:) uri queryparams = uri { uriQuery = queryparams <> (uriQuery uri) }

(=:) :: RText QueryKey -> RText QueryValue -> QueryParam
(=:) k v = QueryParam k v

parseRequestFromUri :: MonadThrow m => URI -> m C.Request
parseRequestFromUri uri = do
  authority <- either (const . throw $ UriException {uriExceptionUri = uri, uriExceptionMessage = "Uri had no valid authority"}) return (uriAuthority uri)
  let host = T.urlEncode False . encodeUtf8  . unRText . authHost $ authority
  scheme <- maybe (throw UriException {uriExceptionUri = uri, uriExceptionMessage = "Uri had no valid scheme"}) return (uriScheme uri)
  let isSecure = (unRText scheme) == "https"
  let port = maybe (if isSecure then 443 else 80) id (authPort authority)
  let path = maybe ([]) (\(b, p) -> NL.toList . fmap unRText $ p) (uriPath uri)
  let queryText = fmap unQueryParam . uriQuery $ uri 
  return $ C.defaultRequest { C.host = host, C.path = LB.toStrict . BB.toLazyByteString . UT.encodePathSegments $ path, C.secure = isSecure, C.port = fromIntegral port, C.queryString = LB.toStrict . BB.toLazyByteString . UT.renderQueryText True $ queryText }
