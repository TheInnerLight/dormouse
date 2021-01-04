{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Dormouse.Status
  ( ok
  , created
  , accepted
  , nonAuthoritativeInformation
  , noContent
  , resetContent
  , partialContent
  , badRequest
  , notFound
  , internalServerError
  , pattern Informational
  , pattern Successful
  , pattern Redirect
  , pattern ClientError
  , pattern ServerError
  , pattern Ok
  , pattern Created
  , pattern Accepted
  , pattern NonAuthoritativeInformation
  , pattern NoContent
  , pattern ResetContent
  , pattern PartialContent
  , pattern BadRequest
  , pattern NotFound
  , pattern InternalServerError
  ) where

-- | Checks whether the status code is in the range of Informational (1xx) status codes
isInformational :: Int -> Bool
isInformational x = x >= 100 && x < 200

-- | Checks whether the status code is in the range of Successful (2xx) status codes
isSuccessful :: Int -> Bool
isSuccessful x = x >= 200 && x < 300

-- | Checks whether the status code is in the range of Redirect (3xx) status codes
isRedirect :: Int -> Bool
isRedirect x = x >= 300 && x < 400

-- | Checks whether the status code is in the range of Client Error (4xx) status codes
isClientError :: Int -> Bool
isClientError x = x >= 400 && x < 500

-- | Checks whether the status code is in the range of Server Error (5xx) status codes
isServerError :: Int -> Bool
isServerError x = x >= 500 && x < 600

ok :: Int
ok = 200

created :: Int
created = 201

accepted :: Int
accepted = 202

nonAuthoritativeInformation :: Int
nonAuthoritativeInformation = 203

noContent :: Int
noContent = 204

resetContent :: Int
resetContent = 205

partialContent :: Int
partialContent  = 206

badRequest :: Int
badRequest = 400

notFound :: Int
notFound = 404

internalServerError :: Int
internalServerError = 500

-- | Matches for 1XX http status codes
pattern Informational :: Int
pattern Informational <- (isInformational -> True)

-- | Matches for 2XX http status codes
pattern Successful :: Int
pattern Successful <- (isSuccessful -> True)

-- | Matches for 3XX http status codes
pattern Redirect :: Int
pattern Redirect <- (isRedirect -> True)

-- | Matches for 4XX http status codes
pattern ClientError :: Int
pattern ClientError <- (isClientError -> True)

-- | Matches for 5XX http status codes
pattern ServerError :: Int
pattern ServerError <- (isServerError -> True)

-- | Matches the 200 Ok status code
pattern Ok :: Int
pattern Ok <- ((==) ok -> True)

-- | Matches the 201 Created status code
pattern Created :: Int
pattern Created <- ((==) created -> True)

-- | Matches the 202 Accepted status code
pattern Accepted :: Int
pattern Accepted <- ((==) accepted -> True)

-- | Matches the 203 Non-Authoritative Information status code
pattern NonAuthoritativeInformation :: Int
pattern NonAuthoritativeInformation <- ((==) nonAuthoritativeInformation -> True)

-- | Matches the 204 No Content status code
pattern NoContent :: Int
pattern NoContent <- ((==) noContent -> True)

-- | Matches the 205 Reset Content status code
pattern ResetContent :: Int
pattern ResetContent <- ((==) resetContent -> True)

-- | Matches the 206 Partial Content status code
pattern PartialContent :: Int
pattern PartialContent <- ((==) partialContent -> True)

-- | Matches the 400 Bad Request status code
pattern BadRequest :: Int
pattern BadRequest <- ((==) badRequest -> True)

-- | Matches the 404 Not Found status code
pattern NotFound :: Int
pattern NotFound <- ((==) notFound -> True)

-- | Matches the 500 Internal Server Error status code
pattern InternalServerError :: Int
pattern InternalServerError <- ((==) internalServerError -> True)


