{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Network.Socket hiding (send, sendTo)
import Network.Socket.ByteString
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
    , ircHost     :: String
    , ircPort     :: Int
    , ircDatabase :: FilePath
    }

startBot :: BotConfig -> IO ()
startBot cfg = connect' cfg >>= runBot (run cfg)

connect' :: BotConfig -> IO BotState
connect' cfg = notify $ do
    let host = ircHost cfg
        port = show $ ircPort cfg
    chan <- newChan
    addr <- head <$> getAddrInfo Nothing (Just host) (Just port)
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock $ addrAddress addr

    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    forkIO . fix $ \loop -> do
        msg <- encode <$> readChan chan
        B.hPutStrLn h msg
        B.putStrLn    msg
        loop

    db    <- DB.connectSqlite3 $ ircDatabase cfg
    time  <- getClockTime

    let readLine = B.hGetLine h
        writer   = writeChan chan
    return $ BotState readLine writer db time
  where
    notify = bracket_
        (B.putStrLn "Connect..." >> hFlush stdout)
        (B.putStrLn "done")

runBot :: Bot a -> BotState -> IO a
runBot = runReaderT . unBot

run :: BotConfig -> Bot ()
run cfg = do
    write $ IRC.nick (ircNick cfg)
    write $ IRC.user (ircNick cfg) "0" "*" (ircRealName cfg)
    write $ IRC.joinChan "#vodik"

    reader <- asks readLine
    forever $ do
        line <- io reader
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