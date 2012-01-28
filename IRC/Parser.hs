module IRC.Parser where

import Control.Applicative hiding ((<|>), optional, many)
import Control.Monad
import Data.Maybe
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Language.Haskell.HsColour.ANSI

-- data Message = Message
--     { prefix  :: Maybe String
--     , cmds :: String
--     , args    :: [String]
--     } deriving (Eq, Ord, Show)

type Parameter  = String
type ServerName = String
type UserName   = String
type RealName   = String
type Command2   = String


-- | IRC messages are parsed as:
--   [ ':' prefix space ] command { space param } crlf
data Message
  = -- | IRC Message
    Message (Maybe Prefix) Command2 [Parameter]
    deriving (Show,Read,Eq)


-- | The optional beginning of an IRC messages
data Prefix = Server ServerName
            | Nick String (Maybe UserName) (Maybe ServerName)
    deriving (Show,Read,Eq)

-- XXX: Shouldn't be in IO Monad
decode :: String -> IO (Maybe Message)
decode s = case parse message "" s of
    Left err -> do
        putStrLn . highlight [ Foreground Yellow ] $ "FAILED: " ++ show err
        return Nothing
    Right msg -> return $ Just msg


-- | The deprecated version of decode
parseMessage = decode

-- | Take all tokens until one character from a given string is found
takeUntil s = anyChar `manyTill` lookAhead (oneOf s)

-- | Convert a parser that consumes all space after it
tokenize p = p >>= \x -> spaces >> return x

-- | Consume only spaces tabs or the bell character
spaces' = skipMany1 (oneOf " \t\b")

prefix :: (Monad m) => ParsecT String u m Prefix
prefix = char ':' >> (try nickPrefix <|> serverPrefix)

serverPrefix :: (Monad m) => ParsecT String u m Prefix
serverPrefix = fmap Server $ takeUntil " "

nickPrefix :: (Monad m) => ParsecT String u m Prefix
nickPrefix = do
  n <- takeUntil " .!@\r\n"
  p <- option False (char '.' >> return True)
  when p (fail "")
  u <- optionMaybe $ char '!' >> takeUntil " @\r\n"
  s <- optionMaybe $ char '@' >> takeUntil " \r\n"
  return $ Nick n u s

-- | Parse a command.  Either a string of capital letters, or 3 digits.
-- command :: CharParser st Command
command = many1 upper
      <|> do x <- digit
             y <- digit
             z <- digit
             return [x,y,z]

-- | Parse a command parameter.
-- parameter :: CharParser st Parameter
parameter = (char ':' >> takeUntil "\r\n")
        <|> takeUntil " \r\n"

-- | Parse a cr lf
-- crlf :: CharParser st ()
crlf = optional (char '\r') >> char '\n' >> return ()

-- | Parse a Message
-- message :: CharParser st Message
message  = do
  p <- optionMaybe $ tokenize prefix
  c <- command
  ps <- many (spaces' >> parameter)
  crlf >> eof
  return $ Message p c ps
