{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (ByteString)
import Network.Bot
import Network.Bot.Base
import Network.Bot.Irc
import Network.Bot.Modules
import Network.IRC

import qualified Data.ByteString.Char8 as B
import qualified Network.IRC.Commands as IRC
import qualified Network.IRC.Commands.Mode as IRC
-- import qualified Network.IRC.Bot.Modules.Echo

myModule :: Module
myModule = mkModule_ "Example" [command]
  where
    command = onBangCommand $ \x xs -> do
        case x of
            "!test"  -> reply "test yourself"
            "!echo"  -> reply (B.unwords xs)
            "!nick"  -> liftM nick readEnv >>= reply
            "!set"   -> write (IRC.nick $ head xs)
            "!part"  -> part $ Just "later gator"
            "!quit"  -> quit $ Just "later gator"
            "!delay" -> reply "will wait!" >> delay 10 >> reply "delayed reply!"
            _        -> return ()
    delay = io . threadDelay . (1000000 *)

myModule2 :: Module
myModule2 = mkModule "Static" [command] (return "Hello World")
  where
    command static = onBangCommand $ \x _ -> do
        case x of
            "!msg" -> reply static >> kick "vodik"
            _      -> return ()

modules :: [Module]
modules = -- [ Modules.Echo.handler ]
    [ myModule
    , myModule2
    ]

main :: IO ()
main = startBot BotConfig
    { ircNick     = "rascal"
    , ircRealName = "Rascal the Bot"
    , ircHost     = "irc.freenode.org"
    , ircPort     = 6667
    , ircDatabase = "rascal.db"
    , ircModules  = modules
    }

withOp :: Irc () -> Irc ()
withOp f = toSender $ \channel -> do
    let op b = write $ IRC.setOp b channel "rascal"
    op True >> f >> op False

kick :: ByteString -> Irc ()
kick nick = withOp . toSender $ \channel ->
    write $ IRC.kick channel nick (Just "what a loser")
