{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Dormouse.Url.QQ
  ( http
  , https
  , url
  ) where

import Data.ByteString.Char8 (pack)
import Dormouse.Url
import Language.Haskell.TH.Quote 
import Language.Haskell.TH

http :: QuasiQuoter
http = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseHttpUrl $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| x |]
  , quotePat = \s ->
      case parseHttpUrl (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Url "http") |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }

https :: QuasiQuoter
https = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseHttpsUrl $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| (x :: Url "https") |]
  , quotePat = \s ->
      case parseHttpsUrl (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Url "https") |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }

url :: QuasiQuoter
url = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseUrl $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| (x : AnyUrl) |]
  , quotePat = \s ->
      case parseUrl (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: AnyUrl) |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }
