{-# LANGUAGE QuasiQuotes #-}

module Uri where

import Dormouse.Uri
import Dormouse.Uri.QQ

telUri :: Uri
telUri = [uri|tel:+1-816-555-1212|]

mailtoUri :: Uri
mailtoUri = [uri|mailto:John.Doe@example.com|]

httpUri :: Uri
httpUri = [uri|http://haskell.org|]
