module IRC.Parser where

import Control.Applicative hiding ((<|>), optional, many)
import Control.Monad
import Data.Maybe
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Language.Haskell.HsColour.ANSI

import IRC.Base

-- XXX: Shouldn't be in IO Monad
decode :: String -> IO (Maybe Message)
decode s = case parse message "" s of
    Left err -> do
        putStrLn . highlight [ Foreground Yellow ] $ "FAILED: " ++ show err
        return Nothing
    Right msg -> return $ Just msg

takeUntil :: (Monad m) => String -> ParsecT String u m String
takeUntil s = anyChar `manyTill` lookAhead (oneOf s)

tokenize :: (Monad m) => ParsecT String u m a -> ParsecT String u m a
tokenize p = p >>= \x -> spaces *> return x

whitespace :: (Monad m) => ParsecT String u m ()
whitespace = skipMany1 $ oneOf " \t\b"

prefix :: (Monad m) => ParsecT String u m Prefix
prefix = char ':' *> (try nickPrefix <|> serverPrefix)

serverPrefix :: (Monad m) => ParsecT String u m Prefix
serverPrefix = Server <$> takeUntil " "

nickPrefix :: (Monad m) => ParsecT String u m Prefix
nickPrefix = do
    n <- takeUntil " .!@\r\n"
    p <- option False $ char '.' *> return True
    when p $ fail ""
    u <- optionMaybe $ char '!' *> takeUntil " @\r\n"
    s <- optionMaybe $ char '@' *> takeUntil " \r\n"
    return $ Nick n u s

command :: (Monad m) => ParsecT String u m String
command = many1 upper <|> count 3 digit

parameter :: (Monad m) => ParsecT String u m String
parameter = (char ':' *> takeUntil "\r\n") <|> takeUntil " \r\n"

crlf :: (Monad m) => ParsecT String u m ()
crlf = optional (char '\r') *> char '\n' *> return ()

message :: (Monad m) => ParsecT String u m Message
message  = do
    p  <- optionMaybe $ tokenize prefix
    c  <- command
    ps <- many (whitespace *> parameter) <* crlf <* eof
    return $ Message p c ps
