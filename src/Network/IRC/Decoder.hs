{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Decoder ( messageParser ) where

import Control.Applicative
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Monoid (mempty)
import Network.IRC.Base
import Data.Attoparsec.Char8

whitespace :: Parser ()
whitespace = skipWhile $ inClass " \n\r"

takeUntil :: String -> Parser ByteString
takeUntil = takeTill . inClass

prefixParser :: Parser Prefix
prefixParser = char8 ':' *> (nickPrefix <|> serverPrefix)

serverPrefix :: Parser Prefix
serverPrefix = ServerPrefix <$> takeUntil " "

nickPrefix :: Parser Prefix
nickPrefix = do
    n <- takeUntil " .!@\r\n"
    p <- option False (char8 '.' *> pure True)
    when p (fail "")
    NickPrefix n <$> optional (char8 '!' *> takeUntil " @\r\n")
                 <*> optional (char8 '@' *> takeUntil " \r\n")

commandParser :: Parser ByteString
commandParser = whitespace *> takeUntil " \r\n"

parameterParser :: Parser ByteString
parameterParser = whitespace *> (trailing <|> middle)
  where
    trailing = char8 ':' *> takeUntil "\r\n"
    middle   = takeUntil " \r\n"

messageParser :: Parser Message
messageParser = Message <$> optional prefixParser
                        <*> commandParser
                        <*> manyTill parameterParser (string "\r\n")
