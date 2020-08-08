# Dormouse

Dormouse is an HTTP client that will help you REST.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad.IO.Class
import Dormouse
import Data.Aeson.TH
import URI.ByteString.QQ (uri)

data UserDetails = UserDetails 
  { name :: String
  , nickname :: String
  , email :: String
  } deriving (Eq, Show)

deriveJSON defaultOptions ''UserDetails
data Echoed a = Echoed 
  { echoeddata :: a
  } deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Echoed

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouse (DormouseConfig { clientManager = manager }) $ do
    let userDetails = UserDetails { name = "James T. Kirk", nickname = "Jim", email = "james.t.kirk@starfleet.com"}
    let req = accept json $ supplyBody json userDetails $ post [uri|https://postman-echo.com/post?ship=enterprise|]
    resp <- sendHttp req
    (response :: Echoed UserDetails) <- decodeBody resp
    liftIO $ print response
    return ()
```
