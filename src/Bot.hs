{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( withBot
    , module Bot.Base
    , module Bot.IRC
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (bracket_)
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Network.IRC
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI (Color(..))
import System.Exit (exitFailure)
import System.IO
import qualified Data.ByteString.Char8 as B8

import Bot.Base
import Bot.IRC

withBot :: BotConfig -> IRC () -> IO ()
withBot cfg irc = do
    sock <- connectIRC cfg

    let writer = liftIO . sendMessage sock
        reader = do
            block <- recv sock 65536
            when (B8.null block) $ error "Socket closed"
            return block

    -- env   <- Environment (ircNick cfg) <$> getCurrentTime
    -- state <- BotState writer messageParser reader <$> newIORef env
    let state = BotState writer messageParser reader

    runBot state $ do
        fromMaybe simpleAuth (ircAuth cfg) cfg
        joinChan $ ircChannels cfg
        run sock irc

connectIRC :: BotConfig -> IO Socket
connectIRC cfg = notify $ do
    let host = ircHost cfg
        port = show $ ircPort cfg

    addr <- head <$> getAddrInfo Nothing (Just host) (Just port)
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock $ addrAddress addr
    return sock
  where
    notify = bracket_ (putStr "Connecting... " >> hFlush stdout)
                      (putStrLn "done")

run :: Socket -> IRC () -> Bot ()
run sock irc = do
    writer <- liftIO $ mkWriter sock

    forever $ do
        msg <- parse
        liftIO . colorize Blue $ print msg
        case command msg of
            "PING"  -> pong msg
            "ERROR" -> liftIO $ putStrLn "ERROR!!!" >> exitFailure
            _       -> void . liftIO . forkIO $ runIRC msg writer irc

mkWriter :: Socket -> IO (Message -> IO ())
mkWriter sock = do
    chan <- newChan
    forkIO . forever $ readChan chan >>= sendMessage sock
    return $ writeChan chan

sendMessage :: Socket -> Message -> IO ()
sendMessage sock msg = print msg >> sendAll sock (encode msg)

simpleAuth :: Authenticator
simpleAuth cfg = do
    nick (ircNick cfg)
    user (ircIdent cfg) "0" "*" (ircRealName cfg)
