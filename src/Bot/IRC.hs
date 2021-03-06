{-# LANGUAGE OverloadedStrings #-}

module Bot.IRC where

import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Network.IRC
import qualified Data.ByteString.Char8 as B8

import Bot.Base

runIRC :: Message -> (Message -> IO ()) -> IRC a -> IO a
runIRC msg writer (IRC a) = runReaderT a $ IRCState msg writer

whenCommand :: ByteString -> IRC () -> IRC ()
whenCommand cmd f = command <$> asks ircMessage >>= flip when f . (cmd ==)

infixr 0 -->

(-->) :: ByteString -> IRC () -> IRC ()
(-->) = whenCommand

getSender :: IRC (Maybe Channel)
getSender = do --asks sender
    c <- asks $ head . parameters . ircMessage
    p <- asks $ prefix . ircMessage
    return $ if B8.head c == '#'
        then Just c
        else case p of
            Just (NickPrefix n _ _) -> Just n
            _                       -> Nothing

bangCommands :: (ByteString -> [ByteString] -> IRC ()) -> IRC ()
bangCommands f = "PRIVMSG" --> do
    (x : xs) <- B8.words <$> argAt 1
    when (B8.head x == '@') $ f x xs

withSender :: (Channel -> IRC ()) -> IRC ()
withSender f = getSender >>= flip whenJust f

part :: Maybe ByteString -> IRC ()
part msg = withSender $ flip partChan msg

reply :: ByteString -> IRC ()
reply msg = withSender $ flip privmsg msg

whenNickPrefix :: (NickName -> Maybe UserName -> Maybe ServerName -> IRC ()) -> IRC ()
whenNickPrefix f = do
    prefix <- asks $ prefix . ircMessage
    case prefix of
        Just (NickPrefix n i h) -> f n i h
        _                       -> return ()

whenServerPrefix :: (ServerName -> IRC ()) -> IRC ()
whenServerPrefix f = do
    prefix <- asks $ prefix . ircMessage
    case prefix of
        Just (ServerPrefix s) -> f s
        _                     -> return ()

args :: IRC [ByteString]
args = asks $ parameters . ircMessage

slice :: Int -> Int -> IRC [ByteString]
slice a b = take b . drop a <$> args

argAt :: Int -> IRC ByteString
argAt a = (!! a) <$> args

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m
