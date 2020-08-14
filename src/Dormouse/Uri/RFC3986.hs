module Dormouse.Uri.RFC3986
  ( isGenDelim
  , isSubDelim
  , isReserved
  , isAsciiAlpha
  , isAsciiAlphaNumeric
  , isUnreserved
  , isSchemeChar
  , isUsernameChar
  , isPasswordChar
  , isRegNameChar
  , isPathChar
  , isQueryChar
  , isFragmentChar
  ) where

import Data.Char as C

isGenDelim :: Char -> Bool
isGenDelim c = c == ':' || c == '/' || c == '?' || c == '#' || c == '[' || c == ']' || c == '@'

isSubDelim :: Char -> Bool
isSubDelim c = c == '!' || c == '$' || c == '&' || c == '\'' || c == '(' || c == ')' || c == '*' || c == '+' || c == ',' || c == ';' || c == '='

isReserved :: Char -> Bool
isReserved c = isGenDelim c || isSubDelim c

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = C.isAlpha c && C.isAscii c

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric c = C.isAlphaNum c && C.isAscii c

isUnreserved :: Char -> Bool
isUnreserved c = isAsciiAlphaNumeric c || c == '-' || c == '.' || c == '_' || c == '~'

isSchemeChar :: Char -> Bool
isSchemeChar c = isAsciiAlphaNumeric c || c == '+' || c == '.' || c == '-'

isUsernameChar :: Char -> Bool
isUsernameChar c = isUnreserved c || isSubDelim c

isPasswordChar :: Char -> Bool
isPasswordChar c = isUnreserved c || isSubDelim c || c == ':'

isRegNameChar :: Char -> Bool
isRegNameChar c = isUnreserved c || isSubDelim c

isPathChar :: Char -> Bool
isPathChar c = isUnreserved c || isSubDelim c || c == ':' || c == '@'

isQueryChar :: Char -> Bool
isQueryChar c = isPathChar c || c == '?' || c == '/'

isFragmentChar :: Char -> Bool
isFragmentChar c = isPathChar c || c == '?' || c == '/'
