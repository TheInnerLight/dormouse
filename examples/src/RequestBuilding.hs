{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module RequestBuilding where

import Dormouse
import Dormouse.Url.QQ
import Dormouse.Url.Builder

githubHttpsUrl :: Url "https"
githubHttpsUrl = [https|https://github.com|]

githubHttpUrl :: Url "http"
githubHttpUrl = [http|http://github.com|]

githubAnyUrl :: AnyUrl
githubAnyUrl = [url|http://github.com|]

dormouseHttpsUrl :: Url "https"
dormouseHttpsUrl = githubHttpsUrl </> "TheInnerLight" </> "dormouse"

searchUrl :: Url "https"
searchUrl = [https|https://google.com|] </> "search" ? "q" =: ("haskell" :: String)

postmanEchoGetUrl :: Url "http"
postmanEchoGetUrl = [http|http://postman-echo.com/get?foo1=bar1&foo2=bar2/|]

postmanEchoGetReq :: HttpRequest (Url "http") "GET" Empty EmptyPayload acceptTag
postmanEchoGetReq = get postmanEchoGetUrl

postmanEchoGetReq' :: HttpRequest (Url "http") "GET" Empty EmptyPayload JsonPayload
postmanEchoGetReq' = accept json $ get postmanEchoGetUrl

postmanEchoPostUrl :: Url "https"
postmanEchoPostUrl = [https|https://postman-echo.com/post|]

postmanEchoPostReq :: HttpRequest (Url "https") "POST" Empty EmptyPayload JsonPayload
postmanEchoPostReq = accept json $ post postmanEchoPostUrl

sendPostmanEchoGetReq :: MonadDormouse m => m ()
sendPostmanEchoGetReq = do
  (_ :: HttpResponse ()) <- expect postmanEchoGetReq'
  return()

