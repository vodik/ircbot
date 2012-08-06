{-# LANGUAGE OverloadedStrings #-}

module Bot.Modules.Admin where

import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Map (Map)
import Data.Monoid
import Network.IRC
import qualified Data.Map as M
import qualified Database.HDBC.Sqlite3 as DB

import Bot
import Bot.Modules

type State = Map ByteString (IRC ())

registry :: DB.Connection -> Module
registry db = mkModule "Admin" [ iface, whois ] (newIORef M.empty)

iface :: IORef State -> IRC ()
iface state = whenNickPrefix $ \nick _ _ -> do
    conts <- liftIO $ readIORef state
    unless (M.member nick conts) $ do
        "PRIVMSG" --> do
            msg <- asks ircMessage
            case parameters msg !! 1 of
                "@register" -> send "WHOIS" [ nick ] >> liftIO (writeIORef state $ M.insert nick (register nick) conts)
                _           -> return ()

whois :: IORef State -> IRC ()
whois state = whenServerPrefix $ \_ -> do
    conts <- liftIO $ readIORef state
    unless (M.null conts) $ do
        nick <- asks $ (!! 1) . parameters . ircMessage
        "330" --> whenJust (M.lookup nick conts) id
        "318" --> liftIO (writeIORef state $ M.delete nick conts)

register :: ByteString -> IRC ()
register nick = do
    cmd <- asks ircMessage
    case take 3 $ parameters cmd of
        [ _, nick, reg ] -> privmsg nick $ "Nick " <> nick <> " registered as " <> reg
        _                -> return ()
