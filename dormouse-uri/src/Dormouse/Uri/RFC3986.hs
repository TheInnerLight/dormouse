module Dormouse.Uri.RFC3986
  ( isGenDelim
  , isSubDelim
  , isReserved
  , isAsciiAlpha
  , isAsciiAlphaNumeric
  , isUnreserved
  , isSchemeChar
  , isUserInfoChar
  , isRegNameChar
  , isPathChar
  , isPathCharNoColon
  , isQueryChar
  , isFragmentChar
  ) where

import Data.Char as C

-- | Checks whether a char belongs to the gen-delims in RFC3986 
isGenDelim :: Char -> Bool
isGenDelim c = c == ':' || c == '/' || c == '?' || c == '#' || c == '[' || c == ']' || c == '@'

-- | Checks whether a char belongs to the sub-delims in RFC3986 
isSubDelim :: Char -> Bool
isSubDelim c = c == '!' || c == '$' || c == '&' || c == '\'' || c == '(' || c == ')' || c == '*' || c == '+' || c == ',' || c == ';' || c == '='

-- | Checks whether a char belongs to the sub-delims in RFC3986 
isReserved :: Char -> Bool
isReserved c = isGenDelim c || isSubDelim c

-- | Checks whether a char is ascii & alpha
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = C.isAlpha c && C.isAscii c

-- | Checks whether a char is ascii & alphanumeric
isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric c = C.isAlphaNum c && C.isAscii c

-- | Checks whether a char belongs to the unreserved group in RFC3986 
isUnreserved :: Char -> Bool
isUnreserved c = isAsciiAlphaNumeric c || c == '-' || c == '.' || c == '_' || c == '~'

-- | Checks whether a char is a valid scheme char in RFC3986
isSchemeChar :: Char -> Bool
isSchemeChar c = isAsciiAlphaNumeric c || c == '+' || c == '.' || c == '-'

-- | Checks whether a char is a valid user info char in RFC3986
isUserInfoChar :: Char -> Bool
isUserInfoChar c = isUnreserved c || isSubDelim c || c == ':'

-- | Checks whether a char is a valid reg-name char in RFC3986
isRegNameChar :: Char -> Bool
isRegNameChar c = isUnreserved c || isSubDelim c

-- | Checks whether a char is a valid path char in RFC3986
isPathChar :: Char -> Bool
isPathChar c = isUnreserved c || isSubDelim c || c == ':' || c == '@'

-- | Checks whether a char is a valid path char (excluding colons) in RFC3986
isPathCharNoColon :: Char -> Bool
isPathCharNoColon c = isUnreserved c || isSubDelim c || c == '@'

-- | Checks whether a char is a valid query char in RFC3986
isQueryChar :: Char -> Bool
isQueryChar c = isPathChar c || c == '?' || c == '/'

-- | Checks whether a char is a valid fragment char in RFC3986
isFragmentChar :: Char -> Bool
isFragmentChar c = isPathChar c || c == '?' || c == '/'
