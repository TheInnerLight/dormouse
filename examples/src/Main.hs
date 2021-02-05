{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.IO.Class
import Dormouse.Client
import Data.Aeson.TH 
import qualified Data.Text as T
import GHC.Generics (Generic)
import Dormouse.Url.QQ (https)
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

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouse (DormouseClientConfig { clientManager = manager }) $ do
    let 
      userDetails = UserDetails 
        { name = "James T. Kirk"
        , nickname = "Jim"
        , email = "james.t.kirk@starfleet.com"
        }
      req = accept json $ supplyBody json userDetails $ post [https|https://postman-echo.com/post|]
      req' = accept html $ get [https|https://google.com|]
    response :: HttpResponse (EchoedJson UserDetails) <- expect req
    liftIO $ print response
    response' :: HttpResponse T.Text <- expectAs html req'
    liftIO $ print response'
    return ()
