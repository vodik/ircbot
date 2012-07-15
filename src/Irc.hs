{-# LANGUAGE OverloadedStrings #-}

module Irc where

import Control.Concurrent
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

commands :: Irc ()
commands = whenCommand "PRIVMSG" $ do
    (x : xs) <- B.words . (!! 1) <$> asks (parameters . msg)
    case x of
        "!test"  -> reply "test yourself"
        "!echo"  -> reply (B.unwords xs)
        "!part"  -> part $ Just "later gator"
        "!quit"  -> quit $ Just "later gator"
        "!delay" -> reply "will wait!" >> delay 10 >> reply "delayed reply!"
        _        -> return ()
  where
    delay = io . threadDelay . (1000000 *)

getChannel :: Irc (Maybe Channel)
getChannel = do
    c <- head <$> asks (parameters . msg)
    p <- asks (prefix . msg)
    return $ if B.head c == '#'
        then Just c
        else case p of
            Just (NickPrefix n _ _) -> Just n
            _                       -> Nothing

onChannel :: (Channel -> Irc ()) -> Irc ()
onChannel f = getChannel >>= flip whenJust f

part :: Maybe ByteString -> Irc ()
part msg = onChannel $ \chan -> write $ IRC.part chan msg

reply :: ByteString -> Irc ()
reply msg = onChannel $ \chan -> write $ IRC.privmsg chan msg
