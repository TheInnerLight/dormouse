{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Dormouse.Uri.QQ
  ( uri
  , uriRef
  ) where
  
import Data.ByteString.Char8 (pack)
import Dormouse.Uri
import Language.Haskell.TH.Quote 
import Language.Haskell.TH 

uri :: QuasiQuoter
uri = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseUri $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| x :: Uri |]
  , quotePat = \s ->
      case parseUri (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: Uri) |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }

uriRef :: QuasiQuoter
uriRef = QuasiQuoter 
  { quoteExp = \s -> 
      let res = parseUriRef $ pack s in
      case res of
        Left err -> fail $ show err
        Right x -> [| x :: UriReference |]
  , quotePat = \s ->
      case parseUriRef (pack s) of
        Left err -> fail $ show err
        Right x  -> appE [|(==)|] [| (x :: UriReference) |] `viewP` [p|True|]
  , quoteType = error "Not supported"
  , quoteDec =error "Not supported"
  }
