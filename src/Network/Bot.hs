{-# LANGUAGE OverloadedStrings #-}

module Network.Bot where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Time
import Database.HDBC.Sqlite3 (connectSqlite3)
import Network.Bot.Base
import Network.Bot.Irc
import Network.Bot.Modules
import Network.IRC
import Network.Socket hiding (send, sendTo)
import Network.Socket.ByteString
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Database.HDBC as DB
import qualified Network.IRC.Commands as IRC

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
    env  <- newIORef $ Environment (ircNick cfg) []
    time <- getCurrentTime

    let reader = do
            line <- B.hGetLine h
            B.putStrLn line
            return $ decode line
        writer = writeChan chan
    return $ BotState reader writer db env time
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

updateNick :: Message -> Bot ()
updateNick msg = do
    e <- readEnv
    case prefix msg of
        Just (NickPrefix n _ _) -> do
            let n' = head $ parameters msg
            io $ B.putStrLn n
            io $ B.putStrLn n'
            when (nick e == n) $
                writeEnv $ e { nick = n' }
            return ()
        Nothing                 -> return ()

handleMessage :: Message -> Bot ()
handleMessage msg = do
    when (msg =? "PING") $ pong msg
    when (msg =? "NICK") $ updateNick msg

    bot <- ask
    hs  <- handlers <$> readEnv
    let state = IrcState msg bot

    forM_ hs $ \v ->
        void . io . forkIO . mapM_ (`runIrc` state) $ hook v
