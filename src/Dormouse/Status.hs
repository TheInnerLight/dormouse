{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Dormouse.Status
  ( ok
  , notFound
  , pattern Ok
  , pattern NotFound
  , pattern Informational
  , pattern Successful
  , pattern Redirect
  , pattern ClientError
  , pattern ServerError
  ) where

ok :: Int
ok = 200

notFound :: Int
notFound = 404

pattern Ok <- ok
pattern NotFound <- notFound

isInformational x = x >= 100 && x < 200
isSuccessful x = x >= 200 && x < 300
isRedirect x = x >= 300 && x < 400
isClientError x = x >= 400 && x < 500
isServerError x = x >= 500 && x < 600

pattern Informational <- (isInformational -> True)
pattern Successful <- (isSuccessful -> True)
pattern Redirect <- (isRedirect -> True)
pattern ClientError <- (isClientError -> True)
pattern ServerError <- (isServerError -> True)
