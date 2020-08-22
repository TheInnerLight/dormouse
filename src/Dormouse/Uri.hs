{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Uri
  ( genClientRequestFromUrl
  , module Dormouse.Uri.Types
  , ensureHttp
  , ensureHttps
  , parseUri
  , parseHttpUrl
  , parseHttpsUrl
  , IsQueryVal(..)
  ) where

import Data.Bifunctor (first)
import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NL
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.CaseInsensitive (CI, foldedCase)
import Data.Proxy
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable, cast)
import Dormouse.Types (SomeDormouseException(..))
import Dormouse.Uri.Parser
import Dormouse.Uri.Types
import Dormouse.Uri.Query
import Dormouse.Uri.Encode
import GHC.TypeLits
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as T
import qualified Network.HTTP.Types.URI as UT

data UriException = forall ref scheme. UriException  {uriExceptionMessage :: Text}

instance Show (UriException) where
  show (UriException { uriExceptionMessage = message }) = "Failed to parse uri: " <> show message

instance Exception (UriException) where
  toException    = toException . SomeDormouseException
  fromException x = do
    SomeDormouseException a <- fromException x
    cast a

-- | Ensure that the supplied Url uses the _http_ scheme, throwing a 'UriException' in @m@ if this is not the case
ensureHttp :: MonadThrow m => AnyUrl -> m (Url "http")
ensureHttp (AnyUrl (HttpUrl u)) = return $ HttpUrl u
ensureHttp (AnyUrl (HttpsUrl u)) = throw $ UriException "Supplied url was an https url, not an http url"

-- | Ensure that the supplied Url uses the _https_ scheme, throwing a 'UriException' in @m@ if this is not the case
ensureHttps :: MonadThrow m => AnyUrl -> m (Url "https")
ensureHttps (AnyUrl (HttpsUrl u)) = return $ HttpsUrl u
ensureHttps (AnyUrl (HttpUrl u)) = throw $ UriException "Supplied url was an http url, not an https url"

-- | Ensure that the supplied Uri is a Url
ensureUrl :: MonadThrow m => Uri -> m AnyUrl
ensureUrl (AbsoluteUri AbsUri {uriScheme = scheme, uriAuthority = maybeAuthority, uriPath = path, uriQuery = query, uriFragment = fragment}) = do
  authority <- maybe (throw $ UriException "Supplied Uri had no authority component") return maybeAuthority
  case unScheme scheme of
    "http"  -> return $ AnyUrl $ HttpUrl UrlComponents { urlAuthority = authority, urlPath = path, urlQuery = query, urlFragment = fragment }
    "https" -> return $ AnyUrl $ HttpsUrl UrlComponents { urlAuthority = authority, urlPath = path, urlQuery = query, urlFragment = fragment }
    s       -> throw $ UriException ("Supplied Uri had a scheme of " <> pack (show s) <> " which was not http or https.")

-- | Parse an ascii 'ByteString' as an absolute uri, throwing a 'UriException' in @m@ if this fails
parseUri :: MonadThrow m => SB.ByteString -> m Uri
parseUri bs = either (throw . UriException . pack) (return) $ parseOnly pUri bs

-- | Parse an ascii 'ByteString' as a url, throwing a 'UriException' in @m@ if this fails
parseUrl :: MonadThrow m => SB.ByteString -> m AnyUrl
parseUrl bs = do
  uri <- parseUri bs
  ensureUrl uri
  
-- | Parse an ascii 'ByteString' as an http url, throwing a 'UriException' in @m@ if this fails
parseHttpUrl :: MonadThrow m => SB.ByteString -> m (Url "http")
parseHttpUrl text = do 
  anyUrl <- parseUrl text
  ensureHttp anyUrl

-- | Parse an ascii 'ByteString' as an https url, throwing a 'UriException' in @m@ if this fails
parseHttpsUrl :: MonadThrow m => SB.ByteString -> m (Url "https")
parseHttpsUrl text = do 
  anyUrl <- parseUrl text
  ensureHttps anyUrl

genClientRequestFromUrlComponents :: Bool -> UrlComponents -> C.Request
genClientRequestFromUrlComponents isSecure (UrlComponents {urlAuthority = authority, urlPath = path, urlQuery = queryParams, urlFragment = fragment}) =
  let host = T.urlEncode False . encodeUtf8 . unHost . authorityHost $ authority
      port = maybe (if isSecure then 443 else 80) id (authorityPort authority)
      queryText = maybe "" (id) $ queryParams in
  C.defaultRequest
    { C.host = host
    , C.path = encodePath path
    , C.secure = isSecure
    , C.port = fromIntegral port
    , C.queryString = encodeQuery queryText
    }


genClientRequestFromUrl :: Url scheme -> C.Request
genClientRequestFromUrl (HttpsUrl uc) = genClientRequestFromUrlComponents True uc
genClientRequestFromUrl (HttpUrl uc)  = genClientRequestFromUrlComponents False uc
