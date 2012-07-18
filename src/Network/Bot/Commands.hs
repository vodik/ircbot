module Network.Bot.Commands where

import Base
import Control.Applicative
import Control.Monad (when, void)
import Data.ByteString (ByteString)
import Data.Monoid (mempty)
import Network.IRC.Base
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as BC

data BangCommand = BangCommand ByteString
                 | PrivateMessage ByteString
                 deriving (Show)

whitespace :: A.Parser ()
whitespace = AC.skipWhile $ AC.inClass " \n\r"

commandPrefix :: ByteString -> A.Parser (ByteString -> BangCommand)
commandPrefix nick = (bangCommand <|> privateMessage) <* whitespace
  where
    bangCommand    = AC.char8 '!'   *> pure BangCommand
    privateMessage = AC.string nick *> AC.char8 ':' *> pure PrivateMessage

commandParser :: ByteString -> A.Parser BangCommand
commandParser nick = commandPrefix nick <*> A.takeByteString

decode :: ByteString -> Bot (Maybe BangCommand)
decode = do
    nick <- getNick
    resultToMaybe . A.parse (commandParser nick)
  where
    resultToMaybe (A.Done _ x)  = Just x
    resultToMaybe (A.Fail {})   = Nothing
    resultToMaybe (A.Partial f) = case f mempty of
        A.Done _ x -> Just x
        _          -> Nothing
