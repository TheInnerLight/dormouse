{-# LANGUAGE OverloadedStrings, GADTs #-}

module Main where

import Control.Monad
import Data.Either
import Criterion.Main
import Criterion.Types
import Dormouse.Uri
import qualified URI.ByteString as UB
import Control.DeepSeq
import qualified Weigh as W
import qualified Data.ByteString as B

instance NFData UB.Authority
instance NFData UB.Host
instance NFData UB.UserInfo
instance NFData UB.SchemaError
instance NFData UB.URIParseError
instance NFData UB.Scheme
instance NFData UB.Port
instance NFData UB.Query

instance NFData (UB.URIRef a) where
  rnf (UB.URI a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e
  rnf (UB.RelativeRef b c d e) = rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance NFData Scheme where
  rnf (Scheme t) = rnf t

instance NFData Username where
  rnf (Username t) = rnf t

instance NFData Password where
  rnf (Password t) = rnf t

instance NFData Host where
  rnf (Host t) = rnf t

instance NFData PathSegment where
  rnf (PathSegment t) = rnf t

instance NFData Query where
  rnf (Query t) = rnf t

instance NFData Fragment where
  rnf (Fragment t) = rnf t

instance NFData (Path a) where
  rnf (Path t) = rnf t

instance NFData UserInfo where
  rnf (UserInfo a b) = rnf a `seq` rnf b

instance NFData Authority where
  rnf (Authority a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData AbsUri where
  rnf (AbsUri a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance NFData RelUri where
  rnf (RelUri a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData Uri where
  rnf (AbsoluteUri a) = rnf a
  rnf (RelativeUri a) = rnf a

uriStrings :: [B.ByteString]
uriStrings = 
  [ "https://john.doe@www.example.com:123/forum/questions/?tag=networking&order=newest#top"
  , "mailto:John.Doe@example.com"
  , "news:comp.infosystems.www.servers.unix"
  , "tel:+1-816-555-1212"
  , "telnet://192.0.2.16:80/"
  , "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
  , "ftp://ftp.is.co.za/rfc/rfc1808.txt"
  , "http://www.ietf.org/rfc/rfc2396.txt"
  , "https://example.com/over/there?name=ferret"
  , "https://example.com/path/to/page?name=ferret&color=purple"
  , "https://example.com/path/to/page?field1=value1&field2=value2&field3=value3"
  , "https://example.com/%E4%B8%8A%E6%B5%B7%2B%E4%B8%AD%E5%9C%8B"
  ]

main :: IO ()
main = do
  defaultMainWith myConfig [
    bgroup "tests" 
      [ bench "dormouse-uri"          . nfAppIO (traverse parseUri) $ uriStrings
      , bench "uri-bytestring strict" . nf (fmap $ UB.parseURI UB.strictURIParserOptions) $ uriStrings
      , bench "uri-bytestring lax"    . nf (fmap $ UB.parseURI UB.laxURIParserOptions) $ uriStrings
      ]
    ]
  W.mainWith $ do
    W.io "dormouse-uri" (traverse parseUri) uriStrings
    W.func "uri-bytestring strict" (fmap $ UB.parseURI UB.strictURIParserOptions) uriStrings
    W.func "uri-bytestring lax" (fmap $ UB.parseURI UB.laxURIParserOptions) uriStrings
  where myConfig = defaultConfig {timeLimit = 5}


