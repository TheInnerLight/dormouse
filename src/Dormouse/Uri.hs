{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Dormouse.Uri
  ( parseRequestFromUri
  , module Dormouse.Uri.Types
  , ensureHttp
  , ensureHttps
  , parseAbsoluteUri
  , parseRelativeUri
  , parseHttpUri
  , parseHttpsUri
  , (//)
  ) where

import Data.Bifunctor (first)
import Control.Exception.Safe (MonadThrow(..), throw, Exception(..), SomeException)
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NL
import Data.Attoparsec.Text (parseOnly)
import Data.CaseInsensitive (CI, foldedCase)
import Data.Proxy
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable, cast)
import Dormouse.Types (SomeDormouseException(..))
import Dormouse.Uri.Parser
import Dormouse.Uri.Types
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

(//) :: Uri ref scheme -> Text -> Uri ref scheme
(//) (AbsoluteUri scheme maybeAuth (Path {unPath = elems}) queryParams fragment) text = (AbsoluteUri scheme maybeAuth (Path {unPath = PathSegment text : elems}) queryParams fragment)
(//) (RelativeUri (Path {unPath = elems}) queryParams fragment) text                  = (RelativeUri (Path {unPath = PathSegment text : elems}) queryParams fragment)

ensureSchemeSymbol :: (KnownSymbol s, MonadThrow m) => Proxy s -> Uri ref scheme -> m (Uri 'Absolute s)
ensureSchemeSymbol prox (uri @ (AbsoluteUri scheme auth path queryParams fragment)) =  
  if (symbolVal prox == (unpack $ unScheme scheme)) then 
    return $ AbsoluteUri scheme auth path queryParams fragment
  else
    throw UriException { uriExceptionMessage = "Supplied Uri had a scheme of " <> (unScheme scheme) <> " which does not match the desired scheme of " <> (pack $ symbolVal prox) }
ensureSchemeSymbol prox (uri @ (RelativeUri _ _ _)) = throw UriException { uriExceptionMessage = "Provided URI was a Relative URI" }
ensureSchemeSymbol prox (AbsOrRelUri underlying) = ensureSchemeSymbol prox underlying

ensureHttp :: MonadThrow m => Uri ref scheme -> m (Uri 'Absolute "http")
ensureHttp uri = ensureSchemeSymbol (Proxy :: Proxy "http") uri

ensureHttps :: MonadThrow m => Uri ref scheme -> m (Uri 'Absolute "https")
ensureHttps uri = ensureSchemeSymbol (Proxy :: Proxy "https") uri

parseAbsoluteUri :: MonadThrow m => Text -> m (Uri 'Absolute scheme)
parseAbsoluteUri text = either (throw . UriException . pack) (return) $ parseOnly pAbsoluteUri text

parseRelativeUri :: MonadThrow m => Text -> m (Uri 'Relative scheme)
parseRelativeUri text = either (throw . UriException . pack) (return) $ parseOnly pRelativeUri text

parseHttpUri :: MonadThrow m => Text -> m (Uri 'Absolute "http")
parseHttpUri text = do
  uri <- parseAbsoluteUri text
  httpUri <- ensureHttp uri
  return httpUri

parseHttpsUri :: MonadThrow m => Text -> m (Uri 'Absolute "https")
parseHttpsUri text = do
  uri <- parseAbsoluteUri text
  httpsUri <- ensureHttps uri
  return httpsUri

parseRequestFromUri :: MonadThrow m => Uri 'Absolute scheme -> m C.Request
parseRequestFromUri (uri @ (AbsoluteUri scheme maybeAuth path queryParams fragment)) = do
  authority <- maybe (throw $ UriException { uriExceptionMessage = "Uri had no valid authority"} ) return maybeAuth
  let host = T.urlEncode False . encodeUtf8 . unHost . authorityHost $ authority
  let isSecure = (unScheme scheme) == "https"
  let port = maybe (if isSecure then 443 else 80) id (authorityPort authority)
  let pathText = fmap unPathSegment $ unPath path
  let queryText = maybe [] (UT.parseQueryText . E.encodeUtf8 . unQuery) $ queryParams
  return $ C.defaultRequest { C.host = host, C.path = LB.toStrict . BB.toLazyByteString . UT.encodePathSegments $ pathText, C.secure = isSecure, C.port = fromIntegral port, C.queryString = LB.toStrict . BB.toLazyByteString . UT.renderQueryText True $ queryText }
