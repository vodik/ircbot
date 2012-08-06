{-# LANGUAGE OverloadedStrings #-}

module Bot.Modules.Admin where

import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.IORef
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Network.IRC
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Database.HDBC.Sqlite3 as DB

import Bot
import Bot.Modules

type State = Map ByteString (IRC ())

registry :: DB.Connection -> Module
registry db = mkModule "Admin" [ iface, whois ] (newIORef M.empty)

iface :: IORef State -> IRC ()
iface state = whenNickPrefix $ \nick _ _ -> do
    conts <- readState state
    when (M.member nick conts) . bangCommands $ cmds conts nick
  where
    cmds conts nick "@register" _ = do
        send "WHOIS" [ nick ]
        writeState state $ M.insert nick (register nick) conts

whois :: IORef State -> IRC ()
whois state = whenServerPrefix $ \_ -> do
    conts <- readState state
    unless (M.null conts) $ do
        nick <- argAt 1
        "330" --> runWhen (M.lookup nick conts)
        "318" --> writeState state (M.delete nick conts)

register :: ByteString -> IRC ()
register nick = do
    args <- slice 1 3
    case args of
        [ nick, reg ] -> privmsg nick $ "Nick " <> nick <> " registered as " <> reg
        _             -> return ()

readState :: IORef State -> IRC State
readState state = liftIO $ readIORef state

writeState :: IORef State -> State -> IRC ()
writeState state = liftIO . writeIORef state

runWhen :: Monad m => Maybe (m ()) -> m ()
runWhen = fromMaybe (return ())
