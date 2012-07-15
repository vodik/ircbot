{-# LANGUAGE OverloadedStrings #-}

module Main where

import Base
import Bot
import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (ByteString)
import Irc
import Modules
import Network.IRC

import qualified Data.ByteString.Char8 as B
import qualified Network.IRC.Commands as IRC
import qualified Modules.Echo

myModule = mkModule_ "Example" [command]
  where
    command = whenCommand "PRIVMSG" $ do
        (x : xs) <- B.words . (!! 1) <$> asks (parameters . msg)
        case x of
            "!test"  -> reply "test yourself"
            "!echo"  -> reply (B.unwords xs)
            "!part"  -> part $ Just "later gator"
            "!quit"  -> quit $ Just "later gator"
            "!delay" -> reply "will wait!" >> delay 10 >> reply "delayed reply!"
            _        -> return ()
    delay = io . threadDelay . (1000000 *)

modules :: [Module]
modules =
    -- [ Modules.Echo.handler
    [ myModule
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
