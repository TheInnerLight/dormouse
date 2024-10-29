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
  , httpUrlAsBS
  , httpsUrlAsBS
  , urlAsBS
  , IsUrl(..)
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.ByteString as SB
import qualified Data.Text as T
import Dormouse.Url.Exception (UrlException(..))
import Dormouse.Uri
import Dormouse.Url.Class
import Dormouse.Url.Types
import qualified Data.Text.Encoding as TE
import Dormouse.Uri.Encode (encodeQuery, encodePath)
import Network.HTTP.Types (urlEncode)

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

httpUrlAsBS :: Url "http" -> SB.ByteString
httpUrlAsBS (HttpUrl httpUrl) = "http://" <> componentsAsBS httpUrl

httpsUrlAsBS :: Url "https" -> SB.ByteString
httpsUrlAsBS (HttpsUrl httpsUrl) = "https://" <> componentsAsBS httpsUrl

urlAsBS :: AnyUrl -> SB.ByteString
urlAsBS (AnyUrl (HttpUrl httpUrl)) = httpUrlAsBS (HttpUrl httpUrl)
urlAsBS (AnyUrl (HttpsUrl httpUrl)) = httpsUrlAsBS (HttpsUrl httpUrl)

componentsAsBS :: UrlComponents -> SB.ByteString
componentsAsBS uc =
  let auth = urlAuthority uc
      uInfo = maybe "" (\ui -> TE.encodeUtf8 (unUserInfo ui) <> "@") (authorityUserInfo auth)
      host = TE.encodeUtf8 $ unHost $ authorityHost auth
      port = maybe "" (\p -> ":" <> TE.encodeUtf8 (T.pack (show p))) (authorityPort auth)
      path = encodePath (urlPath uc)
      queryString = maybe "" encodeQuery (urlQuery uc)
      fragment = maybe "" (\f -> "#" <> urlEncode False (TE.encodeUtf8 (unFragment f))) (urlFragment uc)
  in uInfo <> host <> port <> path <> queryString <> fragment




