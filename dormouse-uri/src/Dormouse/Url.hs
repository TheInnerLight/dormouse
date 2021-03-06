{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Url
  ( module Dormouse.Url.Types
  , ensureHttp
  , ensureHttps
  , parseUrl
  , parseHttpUrl
  , parseHttpsUrl
  , IsUrl(..)
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.ByteString as SB
import qualified Data.Text as T
import Dormouse.Url.Exception (UrlException(..))
import Dormouse.Uri
import Dormouse.Url.Class
import Dormouse.Url.Types

-- | Ensure that the supplied Url uses the _http_ scheme, throwing a 'UrlException' in @m@ if this is not the case
ensureHttp :: MonadThrow m => AnyUrl -> m (Url "http")
ensureHttp (AnyUrl (HttpUrl u)) = return $ HttpUrl u
ensureHttp (AnyUrl (HttpsUrl _)) = throw $ UrlException "Supplied url was an https url, not an http url"

-- | Ensure that the supplied Url uses the _https_ scheme, throwing a 'UrlException' in @m@ if this is not the case
ensureHttps :: MonadThrow m => AnyUrl -> m (Url "https")
ensureHttps (AnyUrl (HttpsUrl u)) = return $ HttpsUrl u
ensureHttps (AnyUrl (HttpUrl _)) = throw $ UrlException "Supplied url was an http url, not an https url"

-- | Ensure that the supplied Uri is a Url
ensureUrl :: MonadThrow m => Uri -> m AnyUrl
ensureUrl Uri {uriScheme = scheme, uriAuthority = maybeAuthority, uriPath = path, uriQuery = query, uriFragment = fragment} = do
  authority <- maybe (throw $ UrlException "Supplied Url had no authority component") return maybeAuthority
  case unScheme scheme of
    "http"  -> return $ AnyUrl $ HttpUrl UrlComponents { urlAuthority = authority, urlPath = path, urlQuery = query, urlFragment = fragment }
    "https" -> return $ AnyUrl $ HttpsUrl UrlComponents { urlAuthority = authority, urlPath = path, urlQuery = query, urlFragment = fragment }
    s       -> throw $ UrlException ("Supplied Url had a scheme of " <> T.pack (show s) <> " which was not http or https.")

-- | Parse an ascii 'ByteString' as a url, throwing a 'UriException' in @m@ if this fails
parseUrl :: MonadThrow m => SB.ByteString -> m AnyUrl
parseUrl bs = do
  url <- parseUri bs
  ensureUrl url
  
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
