{-# LANGUAGE GADTs #-}

module Dormouse.Url.Class 
  ( IsUrl(..)
  ) where

import Data.Text.Encoding (encodeUtf8)
import Dormouse.Uri.Encode
import Dormouse.Uri.Types
import Dormouse.Url.Types
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as T

genClientRequestFromUrlComponents :: Bool -> UrlComponents -> C.Request
genClientRequestFromUrlComponents isSecure (UrlComponents {urlAuthority = authority, urlPath = path, urlQuery = queryParams, urlFragment = _} ) =
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

class (Eq url, Show url) => IsUrl url where
  createRequest :: url -> C.Request

instance IsUrl (Url scheme) where
  createRequest = genClientRequestFromUrl

instance IsUrl (AnyUrl) where
  createRequest (AnyUrl u) = genClientRequestFromUrl u
