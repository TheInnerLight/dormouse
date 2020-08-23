{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Dormouse.Uri.QQ
  ( uri
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
