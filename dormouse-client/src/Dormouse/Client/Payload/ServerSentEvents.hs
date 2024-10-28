module Dormouse.Client.Payload.ServerSentEvents where

import qualified Streamly.Unicode.Parser as SUP
import Control.Applicative ((<|>))
import qualified Streamly.Data.Parser as SP
import qualified Streamly.Data.Fold as SF
import qualified Streamly.Internal.Data.Fold as SF
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import qualified Data.Text as T

lf ::  Monad m => SP.Parser  Char m Char
lf = SUP.char '\n'

cr ::  Monad m => SP.Parser  Char m Char
cr = SUP.char '\r'

space ::  Monad m => SP.Parser  Char m Char
space = SUP.char ' '

colon ::  Monad m => SP.Parser  Char m Char
colon = SUP.char ':'

anyChar ::  Monad m => SP.Parser Char m Char
anyChar = SP.satisfy (\c -> c /= '\r' && c/= '\n')

nameChar ::  Monad m => SP.Parser Char m Char
nameChar = SP.satisfy (\c -> c /= '\r' && c /= '\n' && c /= ':')

eol ::  Monad m => SP.Parser  Char m [Char]
eol = (pure <$> lf) <|> (pure <$> cr) <|> SUP.string "\r\n"

-- many1 ::  Monad m => SP.Parser a m b -> SF.Fold m b c -> SP.Parser a m c
-- many1 parser fold = do
--   first <- parser
--   parser' <- SP.fromEffect $ SF.snoc fold first
--   SP.many parser parser'

-- maybe :: Monad m => 
-- maybe = (Just <$> SP.one ) <|> (pure Nothing)

field ::  Monad m => SP.Parser  Char m ([Char], [Char])
field = do 
  name <- SP.some nameChar SF.toList 
  field' <- SP.many thing SF.one
  _ <- eol
  pure (name, fromJust field')
  where 
    thing = do
      _ <- colon
      _ <- SP.many space SF.drain
      SP.many anyChar SF.toList

comment :: Monad m => SP.Parser  Char m [Char]
comment = do
  _ <- colon
  comment' <- SP.many anyChar SF.toList
  _ <- eol
  pure comment'

data Event 
  = Comment String
  | Field String String

data SSE = SSE
  { sseID :: Int
  , sseData :: ByteString
  , sseEventType :: T.Text
  }

event :: Monad m => SP.Parser Char m Event
event = (Comment <$> comment) <|> (uncurry Field <$> field)











