{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Bot.Auth where

import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Network.IRC
import System.Console.ANSI (Color(..))
import System.Exit (exitFailure)

import Bot.Base
import Bot.Sasl

data SaslType = Plain | DhBlowfish

saslAuth :: SaslType -> ByteString -> ByteString -> Authenticator
saslAuth proto username password cfg = do
    send "CAP" [ "LS" ]
    nick (ircNick cfg)
    user (ircIdent cfg) "0" "*" (ircRealName cfg)

    fix $ \loop -> do
        msg <- parse
        liftIO . colorize Green $ print msg
        handle loop msg
  where
    handle cont cmd@(command -> "CAP") = do
        let nick = ircNick cfg
        case take 2 $ parameters cmd of
            [ "*",  "LS"  ] -> send "CAP"          [ "REQ", "sasl" ] >> cont
            [ nick, "ACK" ] -> send "AUTHENTICATE" [ "DH-BLOWFISH" ] >> cont
            _               -> liftIO exitFailure

    handle cont cmd@(command -> "AUTHENTICATE") = do
        rst <- liftIO . authenticate username password . head $ parameters cmd
        case rst of
            Left  e -> liftIO $ putStrLn ("Error: " <> e) >> exitFailure
            Right r -> send "AUTHENTICATE" [ r ] >> cont

    handle cont (command -> "903") = send "CAP" [ "END" ]
    handle cont (command -> "904") = liftIO exitFailure
    handle cont (command -> "905") = liftIO exitFailure
    handle cont _                  = cont
