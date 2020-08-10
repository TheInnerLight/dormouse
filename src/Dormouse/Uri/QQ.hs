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
import Data.Text (pack)
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
  , quotePat = error "Not supported"
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
  , quotePat = error "Not supported"
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
  , quotePat = error "Not supported"
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
  , quotePat = error "Not supported"
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }


