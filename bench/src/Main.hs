{-# LANGUAGE OverloadedStrings, GADTs, TypeApplications #-}

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
  rnf (UserInfo a) = rnf a

instance NFData Authority where
  rnf (Authority a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData Uri where
  rnf (Uri a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

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
  , "http://www.youtube.com"
  , "http://www.facebook.com"
  , "http://www.baidu.com"
  , "http://www.yahoo.com"
  , "http://www.amazon.com"
  , "http://www.wikipedia.org"
  , "http://www.qq.com"
  , "http://www.google.co.in"
  , "http://www.twitter.com"
  , "http://www.live.com"
  , "http://www.taobao.com"
  , "http://www.bing.com"
  , "http://www.instagram.com"
  , "http://www.weibo.com"
  , "http://www.sina.com.cn"
  , "http://www.linkedin.com"
  , "http://www.yahoo.co.jp"
  , "http://www.msn.com"
  , "http://www.vk.com"
  , "http://www.google.de"
  , "http://www.yandex.ru"
  , "http://www.hao123.com"
  , "http://www.google.co.uk"
  , "http://www.reddit.com"
  , "http://www.ebay.com"
  , "http://www.google.fr"
  , "http://www.t.co"
  , "http://www.tmall.com"
  , "http://www.google.com.br"
  , "http://www.360.cn"
  , "http://www.sohu.com"
  , "http://www.amazon.co.jp"
  , "http://www.pinterest.com"
  , "http://www.netflix.com"
  , "http://www.google.it"
  , "http://www.google.ru"
  , "http://www.microsoft.com"
  , "http://www.google.es"
  , "http://www.wordpress.com"
  , "http://www.gmw.cn"
  , "http://www.tumblr.com"
  , "http://www.paypal.com"
  , "http://www.blogspot.com"
  , "http://www.imgur.com"
  , "http://www.stackoverflow.com"
  , "http://www.aliexpress.com"
  , "http://www.naver.com"
  , "http://www.ok.ru"
  , "http://www.apple.com"
  , "http://www.github.com"
  , "http://www.chinadaily.com.cn"
  , "http://www.imdb.com"
  , "http://www.google.co.kr"
  , "http://www.fc2.com"
  , "http://www.jd.com"
  , "http://www.blogger.com"
  , "http://www.163.com"
  , "http://www.google.ca"
  , "http://www.whatsapp.com"
  , "http://www.amazon.in"
  , "http://www.office.com"
  , "http://www.tianya.cn"
  , "http://www.google.co.id"
  , "http://www.youku.com"
  , "http://www.rakuten.co.jp"
  , "http://www.craigslist.org"
  , "http://www.amazon.de"
  , "http://www.nicovideo.jp"
  , "http://www.google.pl"
  , "http://www.soso.com"
  , "http://www.bilibili.com"
  , "http://www.dropbox.com"
  , "http://www.xinhuanet.com"
  , "http://www.outbrain.com"
  , "http://www.pixnet.net"
  , "http://www.alibaba.com"
  , "http://www.alipay.com"
  , "http://www.microsoftonline.com"
  , "http://www.booking.com"
  , "http://www.googleusercontent.com"
  , "http://www.google.com.au"
  , "http://www.popads.net"
  , "http://www.cntv.cn"
  , "http://www.zhihu.com"
  , "http://www.amazon.co.uk"
  , "http://www.diply.com"
  , "http://www.coccoc.com"
  , "http://www.cnn.com"
  , "http://www.bbc.co.uk"
  , "http://www.twitch.tv"
  , "http://www.wikia.com"
  , "http://www.google.co.th"
  , "http://www.go.com"
  , "http://www.google.com.ph"
  , "http://www.doubleclick.net"
  , "http://www.onet.pl"
  , "http://www.googleadservices.com"
  , "http://www.accuweather.com"
  , "http://www.googleweblight.com"
  , "http://www.answers.yahoo.com"
  ]

main :: IO ()
main = do
  let times = length uriStrings
  defaultMainWith myConfig [
    bgroup "tests" 
      [ bench ("dormouse-uri x" <> show times)          . nf (fmap (parseUri @Maybe)) $ uriStrings
      , bench ("uri-bytestring strict x" <> show times) . nf (fmap $ UB.parseURI UB.strictURIParserOptions) $ uriStrings
      , bench ("uri-bytestring lax x" <> show times)    . nf (fmap $ UB.parseURI UB.laxURIParserOptions) $ uriStrings
      ]
    ]
  W.mainWith $ do
    W.func "dormouse-uri" (fmap (parseUri @Maybe)) uriStrings
    W.func "uri-bytestring strict" (fmap $ UB.parseURI UB.strictURIParserOptions) uriStrings
    W.func "uri-bytestring lax" (fmap $ UB.parseURI UB.laxURIParserOptions) uriStrings
  where myConfig = defaultConfig {timeLimit = 5}


