{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import System.IO
import System.Time
import Network.IRC
import Prelude hiding (catch)
import qualified Database.HDBC.Sqlite3 as DB
import qualified Network.IRC.Commands as IRC

data BotState = BotState
    { readMessage  :: IO (Maybe Message)
    , writeMessage :: Message -> IO ()
    , db           :: DB.Connection
    , startTime    :: ClockTime
    }

data IrcState = IrcState
    { msg :: Message
    , bot :: BotState
    }

newtype Bot a = Bot { unBot :: ReaderT BotState IO a }
              deriving ( Monad, Functor, MonadIO, MonadReader BotState )

newtype Irc a = Irc { unIrc :: ReaderT IrcState IO a }
              deriving ( Monad, Functor, MonadIO, MonadReader IrcState )

class IrcMonad m where
    write :: Message -> m ()

instance IrcMonad Bot where
    write msg = asks writeMessage >>= liftIO . ($ msg)

instance IrcMonad Irc where
    write msg = asks (writeMessage . bot) >>= liftIO . ($ msg)

pong :: IrcMonad m => Message -> m ()
pong = write . IRC.pong

quit :: IrcMonad m => Maybe ByteString -> m ()
quit = write . IRC.quit
