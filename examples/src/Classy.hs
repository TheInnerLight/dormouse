{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Classy where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class
import Dormouse.Client
import Data.Aeson.TH
import GHC.Generics (Generic)
import Dormouse.Url.QQ
import Web.FormUrlEncoded (ToForm(..), FromForm(..))

data UserDetails = UserDetails 
  { name :: String
  , nickname :: String
  , email :: String
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''UserDetails
instance ToForm UserDetails
instance FromForm UserDetails

data EchoedJson a = EchoedJson 
  { echoedjson :: a
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''EchoedJson

data EchoedForm a = EchoedForm 
  { echoedform :: a
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''EchoedForm

sendRequests :: (MonadThrow m, MonadDormouseClient m, MonadIO m) => m ()
sendRequests = do
  let userDetails = UserDetails { name = "James T. Kirk", nickname = "Jim", email = "james.t.kirk@starfleet.com"}
  let req = accept json $ supplyBody json userDetails $ post [https|https://postman-echo.com/post|]
  let req' = accept json $ supplyBody urlForm userDetails $ post [https|https://postman-echo.com/post?ship=enterprise|]
  test :: HttpResponse (EchoedJson UserDetails) <- expectAs json req
  liftIO $ print test
  return ()

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouse (DormouseClientConfig { clientManager = manager }) sendRequests
