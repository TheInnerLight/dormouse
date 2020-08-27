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

pattern Ok :: p
pattern Ok <- ok

pattern NotFound :: p
pattern NotFound <- notFound

isInformational :: (Ord a, Num a) => a -> Bool
isInformational x = x >= 100 && x < 200
isSuccessful :: (Ord a, Num a) => a -> Bool
isSuccessful x = x >= 200 && x < 300
isRedirect :: (Ord a, Num a) => a -> Bool
isRedirect x = x >= 300 && x < 400
isClientError :: (Ord a, Num a) => a -> Bool
isClientError x = x >= 400 && x < 500
isServerError :: (Ord a, Num a) => a -> Bool
isServerError x = x >= 500 && x < 600

-- | Matches for 1XX http status codes
pattern Informational :: (Ord a, Num a) => a
pattern Informational <- (isInformational -> True)
-- | Matches for 2XX http status codes
pattern Successful :: (Ord a, Num a) => a
pattern Successful <- (isSuccessful -> True)
-- | Matches for 3XX http status codes
pattern Redirect :: (Ord a, Num a) => a
pattern Redirect <- (isRedirect -> True)
-- | Matches for 4XX http status codes
pattern ClientError :: (Ord a, Num a) => a
pattern ClientError <- (isClientError -> True)
-- | Matches for 5XX http status codes
pattern ServerError :: (Ord a, Num a) => a
pattern ServerError <- (isServerError -> True)
