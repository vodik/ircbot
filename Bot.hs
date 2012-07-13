{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Network
import System.IO
import System.Time
import Network.IRC
import qualified Data.ByteString.Char8 as B
import qualified Database.HDBC.Sqlite3 as DB
import qualified Network.IRC.Commands as IRC

import Base
import Irc

data BotConfig = BotConfig
    { ircNick     :: ByteString
    , ircRealName :: ByteString
    , ircHost     :: ByteString
    , ircPort     :: Int
    , ircDatabase :: FilePath
    }

startBot :: BotConfig -> IO ()
startBot cfg = bracket (connect cfg) disconnect loop
  where
    disconnect = hClose . socket
    loop       = runBot (run cfg)

runBot :: Bot a -> BotState -> IO a
runBot = runReaderT . unBot

connect :: BotConfig -> IO BotState
connect cfg = notify $ do
    let host = B.unpack $ ircHost cfg
        port = PortNumber . fromIntegral $ ircPort cfg

    h  <- connectTo host port
    hSetBuffering h NoBuffering

    c  <- newChan
    db <- DB.connectSqlite3 $ ircDatabase cfg
    t  <- getClockTime
    return $ BotState h c (writeChan c) db t
  where
    notify = bracket_
        (B.putStrLn "Connect..." >> hFlush stdout)
        (B.putStrLn "done")

run :: BotConfig -> Bot ()
run cfg = do
    reader <- readLoop
    write $ IRC.nick (ircNick cfg)
    write $ IRC.user (ircNick cfg) "0" "*" (ircRealName cfg)
    write $ IRC.joinChan "#vodik"
    listen

readLoop :: Bot ThreadId
readLoop = do
    c <- asks chan
    h <- asks socket
    io . forkIO . fix $ \loop -> do
        msg <- encode <$> readChan c
        B.hPutStrLn h msg
        B.putStrLn    msg
        loop

listen :: Bot ()
listen = do
    h <- asks socket
    forever $ do
        line <- io $ B.hGetLine h
        case decode line of
            Just msg -> do
                io $ B.putStrLn line
                handleMessage msg
            Nothing  -> return ()

handleMessage :: Message -> Bot ()
handleMessage msg =
    case command msg of
        "PING"    -> pong msg
        "PRIVMSG" -> handle msg
        _         -> return ()
  where
    handle msg = do
        state <- IrcState msg <$> ask
        void . io . forkIO $ runIrc commands state
