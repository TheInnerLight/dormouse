{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Classy where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class
import Dormouse
import Dormouse.Class
import Data.Aeson.TH
import GHC.Generics (Generic)
import Dormouse.Uri.QQ
import Web.FormUrlEncoded (ToForm(..), FromForm(..))
import Language.Haskell.TH

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

sendRequests :: (MonadThrow m, MonadDormouse m, MonadIO m) => m ()
sendRequests = do
  let userDetails = UserDetails { name = "James T. Kirk", nickname = "Jim", email = "james.t.kirk@starfleet.com"}
  let req = accept json $ supplyBody json userDetails $ post [https|https://postman-echo.com/post|]
  let req' = accept json $ supplyBody urlForm userDetails $ post [https|https://postman-echo.com/post?ship=enterprise|]
  --resp <- send req
  --(response :: EchoedJson UserDetails) <- decodeBodyAs json resp
  --liftIO $ print response
  --(response' :: EchoedForm UserDetails) <- expectAs json req'
  (test :: HttpResponse (EchoedJson UserDetails))  <- expectAs json req
  --liftIO $ print response'
  return ()

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouse (DormouseConfig { clientManager = manager }) sendRequests
