# Dormouse

Dormouse is an HTTP client that will help you REST.

It was designed with the following objectives in mind:
       
  - HTTP requests and responses should be modelled by a simple, immutable Haskell Record.
  - Real HTTP calls should be made via an abstraction layer (`MonadDormouse`) so testing and mocking is painless.
  - Illegal requests should be unrepresentable, such as HTTP GET requests with a content body.
  - It should be possible to enforce a protocol (e.g. https) at the type level.
  - It should be possible to handle large request and response bodies via constant memory streaming.

Example use:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.IO.Class
import Dormouse
import Data.Aeson.TH 
import Dormouse.Url.QQ

data UserDetails = UserDetails 
  { name :: String
  , nickname :: String
  , email :: String
  } deriving (Eq, Show)

deriveJSON defaultOptions ''UserDetails

data EchoedJson a = EchoedJson 
  { echoedjson :: a
  } deriving (Eq, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''EchoedJson

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouse (DormouseConfig { clientManager = manager }) $ do
    let userDetails = UserDetails { name = "James T. Kirk", nickname = "Jim", email = "james.t.kirk@starfleet.com"}
    let req = accept json $ supplyBody json userDetails $ post [https|https://postman-echo.com/post|]
    response :: HttpResponse (EchoedJson UserDetails) <- expect req
    liftIO $ print response
    return ()
```
