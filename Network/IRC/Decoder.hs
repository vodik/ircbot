module Network.IRC.Decoder ( decode ) where

import Control.Applicative
import Control.Monad (when, void)
import Data.ByteString (ByteString)
import Data.Monoid (mempty)
import Network.IRC.Base
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as BC

whitespace :: A.Parser ()
whitespace = AC.skipWhile (`elem` " \n\r")

crlf :: A.Parser ()
crlf = void $ AC.char8 '\r' *> optional (AC.char8 '\n')

takeUntil :: String -> A.Parser ByteString
takeUntil s = AC.takeTill (`elem` s)

optionMaybe :: A.Parser a -> A.Parser (Maybe a)
optionMaybe f = A.option Nothing $ Just <$> f

prefixParser :: A.Parser Prefix
prefixParser = AC.char8 ':' *> (AC.try nickPrefix <|> serverPrefix)

serverPrefix :: A.Parser Prefix
serverPrefix = ServerPrefix <$> takeUntil " "

nickPrefix :: A.Parser Prefix
nickPrefix = do
    n <- takeUntil " .!@\r\n"
    p <- AC.option False $ AC.char8 '.' *> return True
    when p $ fail ""
    u <- optionMaybe $ AC.char8 '!' *> takeUntil " @\r\n"
    h <- optionMaybe $ AC.char8 '@' *> takeUntil " \r\n"
    return $ NickPrefix n u h

commandParser :: A.Parser ByteString
commandParser = whitespace *> takeUntil " \r\n"

parameterParser :: A.Parser ByteString
parameterParser = whitespace *> (trailing <|> middle)
  where
    trailing = AC.char8 ':' *> takeUntil "\r\n"
    middle   = takeUntil " \r\n"

messageParser :: A.Parser Message
messageParser =
    Message <$> optionMaybe prefixParser
            <*> commandParser
            <*> A.manyTill parameterParser (crlf <|> A.endOfInput)

decode :: ByteString -> Maybe Message
decode = resultToMaybe . A.parse messageParser
  where
    resultToMaybe (A.Done _ x)  = Just x
    resultToMaybe (A.Fail {})   = Nothing
    resultToMaybe (A.Partial f) = case f mempty of
        A.Done _ x -> Just x
        _          -> Nothing
