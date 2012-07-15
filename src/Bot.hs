{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Time
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.Socket hiding (send, sendTo)
import Network.Socket.ByteString
import Network.IRC
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Database.HDBC as DB
import qualified Network.IRC.Commands as IRC

import Base
import Irc
import Modules

data BotConfig = BotConfig
    { ircNick     :: ByteString
    , ircRealName :: ByteString
    , ircHost     :: String
    , ircPort     :: Int
    , ircDatabase :: FilePath
    , ircModules  :: [Module]
    }

startBot :: BotConfig -> IO ()
startBot cfg = connect' cfg >>= runBot (run cfg)

connect' :: BotConfig -> IO BotState
connect' cfg = notify $ do
    let host = ircHost cfg
        port = show $ ircPort cfg
    addr <- head <$> getAddrInfo Nothing (Just host) (Just port)
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock $ addrAddress addr

    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering

    chan <- newChan
    forkIO . fix $ \loop -> do
        msg <- encode <$> readChan chan
        B.hPutStrLn h msg
        B.putStrLn    msg
        loop

    db   <- connectSqlite3 $ ircDatabase cfg
    time <- getCurrentTime
    mods <- newIORef []

    let reader = do
            line <- B.hGetLine h
            B.putStrLn line
            return $ decode line
        writer  = writeChan chan
    return $ BotState reader writer db time mods
  where
    notify = bracket_
        (B.putStr "Connecting... " >> hFlush stdout)
        (B.putStrLn "done")

runBot :: Bot a -> BotState -> IO a
runBot = runReaderT . unBot

run :: BotConfig -> Bot ()
run cfg = do
    setupDB cfg
    loadModules $ ircModules cfg

    write $ IRC.nick (ircNick cfg)
    write $ IRC.user (ircNick cfg) "0" "*" (ircRealName cfg)
    write $ IRC.joinChan "#vodik"

    reader <- asks readMessage
    fix $ \loop -> do
        line <- io reader
        case line of
            Just msg -> handleMessage msg >> loop
            Nothing  -> return ()

setupDB :: BotConfig -> Bot ()
setupDB cfg = do
    withSql $ \conn ->
        DB.run conn "CREATE TABLE IF NOT EXISTS users ( \
                    \  id SERIAL, \
                    \  nick TEXT, host TEXT \
                    \)" []
    return ()

handleMessage :: Message -> Bot ()
handleMessage msg = do
    when (msg =? "PING") $ pong msg

    bot <- ask
    hs  <- io . readIORef $ handlers bot
    let state = IrcState msg bot

    forM_ hs $ \v ->
        void . io . forkIO . mapM_ (`runIrc` state) $ hook v
