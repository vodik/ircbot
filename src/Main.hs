{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bot
import Modules

import qualified Modules.Echo

modules :: [Module]
modules =
    [ Modules.Echo.handler
    ]

main :: IO ()
main = startBot BotConfig
    { ircNick     = "rascal"
    , ircRealName = "Rascal the Bot"
    , ircHost     = "irc.freenode.org"
    , ircPort     = 6667
    , ircDatabase = "rascal.db"
    }
