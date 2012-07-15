{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Base where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Time
import Database.HDBC (IConnection, commit)
import System.IO
import Network.IRC
import Prelude hiding (catch)
import qualified Database.HDBC.Sqlite3 as DB
import qualified Network.IRC.Commands as IRC

data BotState = BotState
    { readMessage  :: IO (Maybe Message)
    , writeMessage :: Message -> IO ()
    , database     :: DB.Connection
    , startTime    :: UTCTime
    }

data IrcState = IrcState
    { msg :: Message
    , bot :: BotState
    }

data Command = BangCommand ByteString [ByteString]
             | DirectMessage ByteString

newtype Bot a = Bot { unBot :: ReaderT BotState IO a }
              deriving ( Monad, Functor, MonadIO, MonadReader BotState )

newtype Irc a = Irc { unIrc :: ReaderT IrcState IO a }
              deriving ( Monad, Functor, MonadIO, MonadReader IrcState )

class MonadIrc m where
    write   :: Message -> m ()
    withSql :: (forall c. IConnection c => c -> IO a) -> m a

instance MonadIrc Bot where
    write msg = asks writeMessage >>= io . ($ msg)
    withSql f = asks database     >>= runSql f

instance MonadIrc Irc where
    write msg = asks (writeMessage . bot) >>= io . ($ msg)
    withSql f = asks (database     . bot) >>= runSql f

runSql :: (MonadIO m, IConnection conn) => (conn -> IO a) -> conn -> m a
runSql f conn = io $ f conn >>= \result -> commit conn >> return result

pong :: MonadIrc m => Message -> m ()
pong = write . IRC.pong

quit :: MonadIrc m => Maybe ByteString -> m ()
quit = write . IRC.quit

io :: MonadIO m => IO a -> m a
io = liftIO
