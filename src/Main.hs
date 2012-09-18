{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (catch)
import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Configurator.Types
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import System.Directory
import System.IO.Error hiding (catch)
import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Network.IRC.Commands as IRC

import Bot
import Bot.Auth

type BanList = Map ByteString [ByteString]

ignoreList :: ByteString -> Bool
ignoreList = flip elem
    [ "$a:tuxhat"
    , "$a:cirrus_minor"
    , "$a:JonathanDoe"
    , "*!*@dslb-188-108-138-103.pools.arcor-ip.net"
    , "$a:SammyF"
    ]

onBanMessage :: MVar BanList -> IRC ()
onBanMessage bans = do
    [ chan, ban, by ] <- slice 1 3
    unless (ignoreList ban) $ addBan chan ban
  where
    addBan chan ban = liftIO . modifyMVar_ bans $ \list ->
        return $ M.alter (f ban) chan list

    f ban (Just list) = Just (ban : list)
    f ban Nothing     = Just [ban]

onMode :: MVar BanList -> IRC ()
onMode bans = do
    args <- args
    case args of
        [ chan, "+o", "beemo" ] -> handleBans chan
        otherwise               -> return ()
  where
    handleBans chan = do
        list <- liftIO $ readMVar bans
        case M.lookup chan list of
            Just bans -> mapM_ (handleBan chan) bans
            Nothing   -> return ()

    handleBan chan ban = do
        write $ IRC.mkMessage "MODE" [ chan, "-b", ban ]
        write $ IRC.mkMessage "MODE" [ chan, "+b", ban ]

main :: IO ()
main = do
    cfg  <- botConfig =<< C.load [ Optional "$(XDG_CONFIG_HOME)/ircbot.cfg" ]
    list <- newMVar M.empty
    withBot_ cfg startBot $ do
        "332"  --> argAt 1 >>= \chan -> write $ IRC.mkMessage "MODE" [ chan, "+b" ]
        "367"  --> onBanMessage list
        "368"  --> argAt 1 >>= \chan -> privmsg "vodik" ("ban list for " <> chan <> " loaded")
        "MODE" --> onMode list
  where
    startBot = joinChan $ channels [ "#archlinux", "#archlinux-offtopic" ]

botConfig :: Config -> IO BotConfig
botConfig cfg = do
    nick  <- C.lookupDefault (ircNick     freenodeConfig) cfg "nick"
    ident <- C.lookupDefault (ircIdent    freenodeConfig) cfg "ident"
    name  <- C.lookupDefault (ircRealName freenodeConfig) cfg "realname"
    user  <- C.lookup cfg "sasl.user"
    pass  <- C.lookup cfg "sasl.pass"

    return freenodeConfig
        { ircNick     = nick
        , ircIdent    = ident
        , ircRealName = name
        , ircAuth     = liftA2 (saslAuth DhBlowfish) user pass
        }
