{-# LANGUAGE OverloadedStrings #-}

module Irc where

import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Network.IRC
import qualified Data.ByteString.Char8 as B
import qualified Network.IRC.Commands as IRC

import Base

runIrc :: Irc a -> IrcState -> IO a
runIrc = runReaderT . unIrc

whenCommand :: ByteString -> Irc () -> Irc ()
whenCommand cmd f = asks msg >>= flip when f . (=? cmd)

commands = undefined

getSender :: Irc (Maybe Channel)
getSender = do
    c <- head <$> asks (parameters . msg)
    p <- asks (prefix . msg)
    return $ if B.head c == '#'
        then Just c
        else case p of
            Just (NickPrefix n _ _) -> Just n
            _                       -> Nothing

toSender :: (Channel -> Irc ()) -> Irc ()
toSender f = getSender >>= flip whenJust f

part :: Maybe ByteString -> Irc ()
part msg = toSender $ write . flip IRC.part msg

reply :: ByteString -> Irc ()
reply msg = toSender $ write . flip IRC.privmsg msg
