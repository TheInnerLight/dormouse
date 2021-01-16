# Dormouse-Uri

Dormouse-Uri provides type safe handling of `Uri`s and `Url`s.

`Uri` sytax is well defined according to [RFC 3986](https://tools.ietf.org/html/rfc3986), Dormouse-Uri parses and encodes `Uri`s according to the syntax defined in this document.

We define `Url` as an absolute URI associated with web resources, the current version of Dormouse-Uri restricts `Url`s to the `http` and `https` schemes.

Dormouse-Uri has the following features:

 - The `Uri`/`Url` data types use `Data.Text` internally, this allows you to freely include percent-decoded characters which will be properly rendered when the `Url`/`Uri` is encoded.
 - Quasiquoters to allow safe construction of `Uri`/`Url`s from string literals.
 - `DataKinds` allow `Url`s to be restricted to the `http` or `https` schemes are the type level.
 - A UrlBuilder syntax to allow type-safe construction/concatenation of `Url`s from their components, e.g. path and query parameters.

 ## Constructing Uris

 ```haskell
{-# LANGUAGE QuasiQuotes #-}

import Dormouse.Uri
import Dormouse.Uri.QQ

telUri :: Uri
telUri = [uri|tel:+1-816-555-1212|]

mailtoUri :: Uri
mailtoUri = [uri|mailto:John.Doe@example.com|]

httpUri :: Uri
httpUri = [uri|http://haskell.org|]
 ```

 ## Constructing Urls

You can construct Urls using the helper QuasiQuoters:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Dormouse.Url
import Dormouse.Url.QQ

githubHttpsUrl :: Url "https"
githubHttpsUrl = [https|https://github.com|]

githubHttpUrl :: Url "http"
githubHttpUrl = [http|http://github.com|]

githubAnyUrl :: AnyUrl
githubAnyUrl = [url|http://github.com|]
```

You can use the Url Builder syntax to modify an existing Url safely to include paths, adding the import:

```haskell
import Dormouse.Url.Builder
```

To allow:

```haskell
dormouseHttpsUrl :: Url "https"
dormouseHttpsUrl = githubHttpsUrl </> "TheInnerLight" </> "dormouse"
```

The Url will be constructed safely so that any characters that wouldn't normally be allowed in a Url path are percent-encoded before the url is resolved by Dormouse.

You can also handle query parameters using similar syntax:

```haskell
searchUrl :: Url "https"
searchUrl = [https|https://google.com|] </> "search" ? "q" =: ("haskell" :: String)
```
