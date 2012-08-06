{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Monoid
import Network.IRC
import qualified Data.Map as M
import qualified Database.HDBC.Sqlite3 as DB

import Bot
import Bot.Auth
import Bot.Modules
import Bot.Modules.Admin

class IAuthority a where
    allowed :: a -> ByteString -> ByteString -> Bool
    allowed _ _ _ = False

----------------------------------------------------------------------------------------------

startDB :: String -> IO DB.Connection
startDB = DB.connectSqlite3

----------------------------------------------------------------------------------------------

commands :: Module
commands = mkModule_ "commands" [cmds]
  where
    cmds = "PRIVMSG" --> do
        msg <- asks ircMessage
        case parameters msg !! 1 of
            "@test"   -> reply "test yourself"
            "@source" -> reply "https://github.com/vodik/ircbot"
            "@delay"  -> reply "will wait!" >> wait 10 >> reply "delayed reply!"
            _         -> return ()
    wait = io . threadDelay . (1000000 *)

----------------------------------------------------------------------------------------------

main :: IO ()
main = do
    db      <- startDB "bot.db"
    conf    <- botConfig "config.ini"
    modules <- mapM initModule [ registry db, commands ]
    withBot conf $ mapM_ hook modules

botConfig :: String -> IO BotConfig
botConfig config =
    return BotConfig { ircNick     = "beemo"
                     , ircIdent    = "bot"
                     , ircRealName = "Beemo"
                     , ircHost     = "irc.freenode.org"
                     , ircPort     = 6667
                     , ircAuth     = Just $ saslAuth DhBlowfish username password
                     }
  where
    username = undefined
    password = undefined

----------------------------------------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO
