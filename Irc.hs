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

commands :: Irc ()
commands = do
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

getChannel :: Irc Channel
getChannel = do
    c <- head <$> asks (parameters . msg)
    p <- asks (prefix . msg)
    return $ if c /= "boticus"
        then c
        else case p of
            Just (NickPrefix n _ _) -> n
            _                       -> c

part :: Maybe ByteString -> Irc ()
part msg = getChannel >>= write . flip IRC.part msg

reply :: ByteString -> Irc ()
reply msg = getChannel >>= write . flip IRC.privmsg msg

io :: MonadIO m => IO a -> m a
io = liftIO

-------------------------------------------------------------------

-- initialize :: Irc ()
-- initialize = do
--     withSql $ \conn ->
--         DB.run conn "CREATE IF NOT EXISTS TABLE seen ( \
--                     \ id SERIAL, \
--                     \ host TEXT, channel TEXT, \
--                     \ sender TEXT, time TEXT, text TEXT \
--                     \)" []
--     return ()
