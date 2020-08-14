{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Dormouse.Uri.QQ
  ( http
  , https
  , absoluteUri
  , relativeUri
  ) where
import Data.Bifunctor
import Data.ByteString.Char8 (pack)
import Dormouse.Uri
import Dormouse.Uri.Types
import Language.Haskell.TH.Quote 
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax

http :: QuasiQuoter
http = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseHttpUri $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| x |]
  , quotePat = \s ->
      case parseHttpUri (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Uri 'Absolute "http") |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }

https :: QuasiQuoter
https = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseHttpsUri $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| (x :: Uri 'Absolute "https") |]
  , quotePat = \s ->
      case parseHttpsUri (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Uri 'Absolute "https") |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }

absoluteUri :: QuasiQuoter
absoluteUri = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseAbsoluteUri $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| x :: Uri 'Absolute scheme |]
  , quotePat = \s ->
      case parseAbsoluteUri (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Uri 'Absolute scheme) |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }

relativeUri :: QuasiQuoter
relativeUri = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseRelativeUri $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| x :: Uri 'Relative scheme |]
  , quotePat = \s ->
      case parseRelativeUri (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Uri 'Relative scheme) |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }


