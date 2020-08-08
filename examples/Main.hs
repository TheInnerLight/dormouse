{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Dormouse
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Aeson.TH
import Data.Char (toLower)
import URI.ByteString (URI)
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

deriveJSON defaultOptions{fieldLabelModifier = drop 6, constructorTagModifier = map toLower} ''Echoed

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouse (DormouseConfig { clientManager = manager }) $ do
    let userDetails = UserDetails { name = "James T. Kirk", nickname = "Jim", email = "james.t.kirk@starfleet.com"}
    let req = accept json $ supplyBody json userDetails $ post [uri|https://postman-echo.com/post?ship=enterprise|]
    _ <- liftIO $ print req
    resp <- sendHttp req
    (blurg :: Echoed UserDetails) <- decodeBody resp
    _ <- liftIO $ print blurg
    return ()
